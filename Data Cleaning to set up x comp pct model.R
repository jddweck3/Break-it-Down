library(tidyverse)
library(matrixStats)
library(gganimate)
library(av)
library(xgboost)

#Pass in data
setwd("C:/Users/Joshua/Desktop/BDB 2024-25/nfl-big-data-bowl-2025/")
starter <- Sys.time()
data_spot <- "C:/Users/Joshua/Desktop/BDB 2024-25/nfl-big-data-bowl-2025/"
tracking <- data.table::fread("C:/Users/Joshua/Desktop/BDB 2024-25/nfl-big-data-bowl-2025/Combined Tracking Data.csv") %>%
  mutate(a2 = a) %>%
  group_by(gameId, playId, nflId) %>%
  arrange(nflId, gameId, playId, frameId) %>%
  mutate(a = (s - lag(s)) * 10) %>%
  ungroup() %>%
  arrange(gameId, playId, frameId, nflId)

#Get ball tracking data at throw time
throw_time <- tracking %>%
  filter(club %in% c("football", "Football"), event == "pass_forward") %>%
  select(gameId, playId, frameId) %>%
  rename(throw_frame = frameId)

#Get ball tracking data at snap time
snap_time <- tracking %>%
  filter(club %in% c("football", "Football"), frameType == "SNAP") %>%
  select(gameId, playId, frameId) %>%
  rename(snap_frame = frameId)

#Merge together snap time and throw time ball tracking data
snap_throw <- merge(snap_time, throw_time, by = c("gameId", "playId")) %>%
  distinct(gameId, playId, .keep_all = T)

#Merge snap and throw time ball tracking data into overall player tracking data
tracking <- merge(tracking, snap_throw, by = c("gameId", "playId"))

#Pass in plays and convert dropback types into dummy variable
plays <- read_csv(paste0(data_spot, "plays.csv")) %>%
  mutate(dropback_type = case_when(dropbackType == "DESIGNED_ROLLOUT_LEFT" ~ 0,
                                   dropbackType == "DESIGNED_ROLLOUT_RIGHT" ~ 1,
                                   dropbackType == "SCRAMBLE" ~ 2,
                                   dropbackType == "SCRAMBLE_ROLLOUT_LEFT" ~ 3,
                                   dropbackType == "SCRAMBLE_ROLLOUT_RIGHT" ~ 4,
                                   dropbackType == "TRADITIONAL" ~ 5,
                                   T ~ 6))


players <- read_csv(paste0(data_spot, "players.csv"))
in_heights <- c()
for(i in players$height){
  if(grepl("-", i)){
    in_heights <- c(in_heights, as.numeric(strsplit(i, "-")[[1]][1]) * 12 + as.numeric(strsplit(i, "-")[[1]][2]))
  } else{
    in_heights <- c(in_heights, as.numeric(i))
  }
}
player_heights <- players %>%
  mutate(height_inches = in_heights) %>%
  select(nflId, height_inches, position)

player_plays <- read_csv("C:/Users/Joshua/Desktop/BDB 2024-25/nfl-big-data-bowl-2025/player_play.csv")

track_plays <- merge(tracking, plays, by = c("gameId", "playId")) %>%
  merge(player_plays %>% select(routeRan, gameId, playId, nflId), by = c("gameId", "playId", "nflId")) %>%
  mutate(gameplay = paste(gameId, playId),
         dir = ifelse(dir == 360, 0, dir),
         adj_dir = round(180 + (360 * floor(dir/180)) - dir, 2),
         adj_dir_rad = adj_dir * pi / 180,
         #The next line converts 360 degree orientations to 0. Since the orientation goes in a circle, they are the same.
         o = ifelse(o == 360, 0, o),
         adj_o = round(180 + (360 * floor(o/180)) - o, 2),
         adj_o_rad = adj_o * pi / 180,
         gl_x = ifelse(playDirection == "left", 10, 110),
         x_to_gl = ifelse(playDirection == "left", x - gl_x, gl_x - x),
         absolute_1d = as.numeric(absoluteYardlineNumber) - as.numeric(yardsToGo),
         x_to_1d = ifelse(gl_x == 110, absolute_1d - x, x - absolute_1d),
         x_to_los = -(ifelse(playDirection == "right", x - absoluteYardlineNumber, absoluteYardlineNumber - x)),
         route_throw = case_when(routeRan == "ANGLE" ~ 0,
                                 routeRan == "CORNER" ~ 1,
                                 routeRan == "CROSS" ~ 2,
                                 routeRan == "FLAT" ~ 3,
                                 routeRan == "GO" ~ 4,
                                 routeRan == "HITCH" ~ 5,
                                 routeRan == "IN" ~ 6,
                                 routeRan == "OUT" ~ 7,
                                 routeRan == "POST" ~ 8,
                                 routeRan == "SCREEN" ~ 9,
                                 routeRan == "SLANT" ~ 10,
                                 routeRan == "WHEEL" ~ 11,
                                 routeRan == "undefined" ~ 12,
                                 is.na(routeRan) ~ -1,
                                 T ~ 13),
         y_to_sl = ifelse(y >= 160/6, 160/3-y, y),
         absolute_1d = ifelse(gl_x == 10, as.numeric(absoluteYardlineNumber) - as.numeric(yardsToGo), as.numeric(absoluteYardlineNumber) + as.numeric(yardsToGo)))

track_plays_players <- merge(track_plays, player_heights, by = c("nflId"))

qbs <- track_plays_players %>%
  filter(position == "QB") %>%
  mutate(qb_name = sub(".*\\b(\\w+)$", "\\1", displayName),
         passer_name = sub(".*?\\b(\\w+)\\s+pass.*", "\\1", playDescription)) %>%
  filter(qb_name == passer_name) %>%
  distinct(gameId, playId, frameId, nflId, .keep_all = TRUE) %>%
  rename(qb_speed = s,
         qb_to_los = x_to_los,
         qb_to_sl = y_to_sl) %>%
  mutate(qb_spd_x = qb_speed * sin(adj_dir_rad),
         qb_spd_y = qb_speed * -cos(adj_dir_rad))


the_ball <- tracking %>%
  filter(is.na(nflId)) %>%
  group_by(gameId, playId) %>%
  summarize(max_velo = max(s),
         frame_max = which.max(s))

qb_ball <- merge(qbs, the_ball, by = c("gameId", "playId"))

qb_ball <- qb_ball %>%
  group_by(nflId) %>%
  mutate(qb_hardest = max(max_velo, na.rm = T)) %>%
  ungroup() %>%
  mutate(dropback_type = case_when(dropbackType == "DESIGNED_ROLLOUT_LEFT" ~ 0,
                                   dropbackType == "DESIGNED_ROLLOUT_RIGHT" ~ 1,
                                   dropbackType == "SCRAMBLE" ~ 2,
                                   dropbackType == "SCRAMBLE_ROLLOUT_LEFT" ~ 3,
                                   dropbackType == "SCRAMBLE_ROLLOUT_RIGHT" ~ 4,
                                   dropbackType == "TRADITIONAL" ~ 5,
                                   T ~ 6))
View(qb_ball %>% group_by(nflId) %>% summarize(name = last(displayName), maxer = first(qb_hardest)))

rm(player_plays, qbs)
gc()

pot_targets <- track_plays_players %>%
  filter(!(is.na(routeRan)), routeRan != "undefined") %>%
  rename(target_a = a,
         target_s = s,
         target_adj_o = adj_o,
         target_adj_dir = adj_dir,
         target_height = height_inches,
         tgt_to_1d = x_to_1d,
         tgt_to_gl = x_to_gl,
         tgt_to_sl = y_to_sl,
         tgt_x = x,
         tgt_y = y,
         tgt_id = nflId,
         tgt_name = displayName,
         tgt_jerseyNumber = jerseyNumber) %>%
  mutate(target_spd_x = target_s * sin(adj_dir_rad),
         target_spd_y = target_s * -cos(adj_dir_rad)) %>%
  select(gameId, playId, frameId, tgt_id, tgt_name, target_a, target_s, target_height, tgt_to_1d, tgt_to_gl, tgt_to_sl, tgt_x, tgt_y, target_adj_o, target_adj_dir, target_spd_x, target_spd_y, tgt_jerseyNumber, route_throw, routeRan)

qb_ball_targets <- merge(qb_ball %>% select(-route_throw, -routeRan), pot_targets, by = c("gameId", "playId", "frameId")) %>%
  rename(qb_x = x, qb_y = y, qb_a = a, qb_o = o, qb_dir = dir, qb_adj_o = adj_o, qb_adj_o_rad = adj_o_rad, qb_adj_dir = adj_dir, qb_adj_dir_rad = adj_dir_rad, qb_to_gl = x_to_gl, qb_to_1d = x_to_1d)

qb_ball_targets <- qb_ball_targets %>%
  mutate(qb_x_target = abs(qb_x - tgt_x),
         qb_y_target = abs(qb_y - tgt_y),
         qb_to_target = sqrt(qb_x_target ** 2 + qb_y_target ** 2),
         qb_target_angle = atan(qb_y_target/qb_x_target),
         qb_tgt_x_rel_spd = qb_spd_x - target_spd_x,
         qb_tgt_y_rel_spd = qb_spd_y - target_spd_y)

games <- read_csv(paste0(data_spot, "games.csv")) %>%
  select(gameId, homeTeamAbbr)

rm(track_plays)

defenders1 <- tracking %>%
  merge(games, by = c("gameId")) %>%
  merge(plays, by = c("gameId", "playId")) %>%
  mutate(is_offense = ifelse(club == possessionTeam, 1, 0),
         adj_dir = round(180 + (360 * floor(dir/180)) - dir, 2),
         adj_dir_rad = adj_dir * pi / 180) %>%
  filter(is_offense == 0, !(is.na(nflId))) %>%
  distinct(gameId, playId, frameId, nflId, .keep_all = TRUE)

defenders1 <- defenders1 %>%
  arrange(x) %>%
  group_by(gameId, playId, frameId) %>%
  mutate(x_rank = row_number()) %>%
  arrange(gameId, playId, frameId, x_rank)

max(defenders1$x_rank)

defender_split_1 <- defenders1 %>%
  filter(x_rank == 1) %>%
  rename(d1_x = x,
         d1_y = y,
         d1_s = s,
         d1_id = nflId,
         d1_adj_dir = adj_dir) %>%
  mutate(d1_spd_x = d1_s * sin(adj_dir_rad),
         d1_spd_y = d1_s * -cos(adj_dir_rad),
         d1_a_x = a * sin(adj_dir_rad),
         d1_a_y = a * -cos(adj_dir_rad)) %>%
  select(gameId, playId, frameId, d1_id, d1_x, d1_y, d1_s, d1_spd_x, d1_spd_y, d1_a_x, d1_a_y, d1_adj_dir) %>%
  arrange(gameId, playId, frameId, d1_id)

defender_split_2 <- defenders1 %>%
  filter(x_rank == 2) %>%
  rename(d2_x = x,
         d2_y = y,
         d2_s = s,
         d2_id = nflId,
         d2_adj_dir = adj_dir) %>%
  mutate(d2_spd_x = d2_s * sin(adj_dir_rad),
         d2_spd_y = d2_s * -cos(adj_dir_rad),
         d2_a_x = a * sin(adj_dir_rad),
         d2_a_y = a * -cos(adj_dir_rad)) %>%
  select(gameId, playId, frameId, d2_id, d2_x, d2_y, d2_s, d2_spd_x, d2_spd_y, d2_a_x, d2_a_y, d2_adj_dir) %>%
  arrange(gameId, playId, frameId, d2_id)

defender_split_3 <- defenders1 %>%
  filter(x_rank == 3) %>%
  rename(d3_x = x,
         d3_y = y,
         d3_s = s,
         d3_id = nflId,
         d3_adj_dir = adj_dir) %>%
  mutate(d3_spd_x = d3_s * sin(adj_dir_rad),
         d3_spd_y = d3_s * -cos(adj_dir_rad),
         d3_a_x = a * sin(adj_dir_rad),
         d3_a_y = a * -cos(adj_dir_rad)) %>%
  select(gameId, playId, frameId, d3_id, d3_x, d3_y, d3_s, d3_spd_x, d3_spd_y, d3_a_x, d3_a_y, d3_adj_dir) %>%
  arrange(gameId, playId, frameId, d3_id)

defender_split_4 <- defenders1 %>%
  filter(x_rank == 4) %>%
  rename(d4_x = x,
         d4_y = y,
         d4_s = s,
         d4_id = nflId,
         d4_adj_dir = adj_dir) %>%
  mutate(d4_spd_x = d4_s * sin(adj_dir_rad),
         d4_spd_y = d4_s * -cos(adj_dir_rad),
         d4_a_x = a * sin(adj_dir_rad),
         d4_a_y = a * -cos(adj_dir_rad)) %>%
  select(gameId, playId, frameId, d4_id, d4_x, d4_y, d4_s, d4_spd_x, d4_spd_y, d4_a_x, d4_a_y, d4_adj_dir) %>%
  arrange(gameId, playId, frameId, d4_id)

defender_split_5 <- defenders1 %>%
  filter(x_rank == 5) %>%
  rename(d5_x = x,
         d5_y = y,
         d5_s = s,
         d5_id = nflId,
         d5_adj_dir = adj_dir) %>%
  mutate(d5_spd_x = d5_s * sin(adj_dir_rad),
         d5_spd_y = d5_s * -cos(adj_dir_rad),
         d5_a_x = a * sin(adj_dir_rad),
         d5_a_y = a * -cos(adj_dir_rad)) %>%
  select(gameId, playId, frameId, d5_id, d5_x, d5_y, d5_s, d5_spd_x, d5_spd_y, d5_a_x, d5_a_y, d5_adj_dir) %>%
  arrange(gameId, playId, frameId, d5_id)

defender_split_6 <- defenders1 %>%
  filter(x_rank == 6) %>%
  rename(d6_x = x,
         d6_y = y,
         d6_s = s,
         d6_id = nflId,
         d6_adj_dir = adj_dir) %>%
  mutate(d6_spd_x = d6_s * sin(adj_dir_rad),
         d6_spd_y = d6_s * -cos(adj_dir_rad),
         d6_a_x = a * sin(adj_dir_rad),
         d6_a_y = a * -cos(adj_dir_rad)) %>%
  select(gameId, playId, frameId, d6_id, d6_x, d6_y, d6_s, d6_spd_x, d6_spd_y, d6_a_x, d6_a_y, d6_adj_dir) %>%
  arrange(gameId, playId, frameId, d6_id)

defender_split_7 <- defenders1 %>%
  filter(x_rank == 7) %>%
  rename(d7_x = x,
         d7_y = y,
         d7_s = s,
         d7_id = nflId,
         d7_adj_dir = adj_dir) %>%
  mutate(d7_spd_x = d7_s * sin(adj_dir_rad),
         d7_spd_y = d7_s * -cos(adj_dir_rad),
         d7_a_x = a * sin(adj_dir_rad),
         d7_a_y = a * -cos(adj_dir_rad)) %>%
  select(gameId, playId, frameId, d7_id, d7_x, d7_y, d7_s, d7_spd_x, d7_spd_y, d7_a_x, d7_a_y, d7_adj_dir) %>%
  arrange(gameId, playId, frameId, d7_id)

defender_split_8 <- defenders1 %>%
  filter(x_rank == 8) %>%
  rename(d8_x = x,
         d8_y = y,
         d8_s = s,
         d8_id = nflId,
         d8_adj_dir = adj_dir) %>%
  mutate(d8_spd_x = d8_s * sin(adj_dir_rad),
         d8_spd_y = d8_s * -cos(adj_dir_rad),
         d8_a_x = a * sin(adj_dir_rad),
         d8_a_y = a * -cos(adj_dir_rad)) %>%
  select(gameId, playId, frameId, d8_id, d8_x, d8_y, d8_s, d8_spd_x, d8_spd_y, d8_a_x, d8_a_y, d8_adj_dir) %>%
  arrange(gameId, playId, frameId, d8_id)

defender_split_9 <- defenders1 %>%
  filter(x_rank == 9) %>%
  rename(d9_x = x,
         d9_y = y,
         d9_s = s,
         d9_id = nflId,
         d9_adj_dir = adj_dir) %>%
  mutate(d9_spd_x = d9_s * sin(adj_dir_rad),
         d9_spd_y = d9_s * -cos(adj_dir_rad),
         d9_a_x = a * sin(adj_dir_rad),
         d9_a_y = a * -cos(adj_dir_rad)) %>%
  select(gameId, playId, frameId, d9_id, d9_x, d9_y, d9_s, d9_spd_x, d9_spd_y, d9_a_x, d9_a_y, d9_adj_dir) %>%
  arrange(gameId, playId, frameId, d9_id)

defender_split_10 <- defenders1 %>%
  filter(x_rank == 10) %>%
  rename(d10_x = x,
         d10_y = y,
         d10_s = s,
         d10_id = nflId,
         d10_adj_dir = adj_dir) %>%
  mutate(d10_spd_x = d10_s * sin(adj_dir_rad),
         d10_spd_y = d10_s * -cos(adj_dir_rad),
         d10_a_x = a * sin(adj_dir_rad),
         d10_a_y = a * -cos(adj_dir_rad)) %>%
  select(gameId, playId, frameId, d10_id, d10_x, d10_y, d10_s, d10_spd_x, d10_spd_y, d10_a_x, d10_a_y, d10_adj_dir) %>%
  arrange(gameId, playId, frameId, d10_id)

defender_split_11 <- defenders1 %>%
  filter(x_rank == 11) %>%
  rename(d11_x = x,
         d11_y = y,
         d11_s = s,
         d11_id = nflId,
         d11_adj_dir = adj_dir) %>%
  mutate(d11_spd_x = d11_s * sin(adj_dir_rad),
         d11_spd_y = d11_s * -cos(adj_dir_rad),
         d11_a_x = a * sin(adj_dir_rad),
         d11_a_y = a * -cos(adj_dir_rad)) %>%
  select(gameId, playId, frameId, d11_id, d11_x, d11_y, d11_s, d11_spd_x, d11_spd_y, d11_a_x, d11_a_y, d11_adj_dir) %>%
  arrange(gameId, playId, frameId, d11_id)
print(Sys.time())

merged_defenders <- Reduce(function(x, y) merge(x, y, by = c("gameId", "playId", "frameId"), all = TRUE), list(defender_split_1, defender_split_2, defender_split_3, defender_split_4, defender_split_5, defender_split_6, defender_split_7, 
                                                                                   defender_split_8, defender_split_9, defender_split_10, defender_split_11))
print(Sys.time())

qb_ball_target_defs <- merge(qb_ball_targets, merged_defenders, by = c("gameId", "playId", "frameId"))  

qb_ball_target_defs <- qb_ball_target_defs %>%
  mutate(d1_x_tgt = abs(d1_x - tgt_x),
         d2_x_tgt = abs(d2_x - tgt_x),
         d3_x_tgt = abs(d3_x - tgt_x),
         d4_x_tgt = abs(d4_x - tgt_x),
         d5_x_tgt = abs(d5_x - tgt_x),
         d6_x_tgt = abs(d6_x - tgt_x),
         d7_x_tgt = abs(d7_x - tgt_x),
         d8_x_tgt = abs(d8_x - tgt_x),
         d9_x_tgt = abs(d9_x - tgt_x),
         d10_x_tgt = abs(d10_x - tgt_x),
         d11_x_tgt = abs(d11_x - tgt_x),
         d1_y_tgt = abs(d1_y - tgt_y),
         d2_y_tgt = abs(d2_y - tgt_y),
         d3_y_tgt = abs(d3_y - tgt_y),
         d4_y_tgt = abs(d4_y - tgt_y),
         d5_y_tgt = abs(d5_y - tgt_y),
         d6_y_tgt = abs(d6_y - tgt_y),
         d7_y_tgt = abs(d7_y - tgt_y),
         d8_y_tgt = abs(d8_y - tgt_y),
         d9_y_tgt = abs(d9_y - tgt_y),
         d10_y_tgt = abs(d10_y - tgt_y),
         d11_y_tgt = abs(d11_y - tgt_y),
         d1_to_tgt = sqrt(d1_x_tgt ** 2 + d1_y_tgt ** 2),
         d2_to_tgt = sqrt(d2_x_tgt ** 2 + d2_y_tgt ** 2),
         d3_to_tgt = sqrt(d3_x_tgt ** 2 + d3_y_tgt ** 2),
         d4_to_tgt = sqrt(d4_x_tgt ** 2 + d4_y_tgt ** 2),
         d5_to_tgt = sqrt(d5_x_tgt ** 2 + d5_y_tgt ** 2),
         d6_to_tgt = sqrt(d6_x_tgt ** 2 + d6_y_tgt ** 2),
         d7_to_tgt = sqrt(d7_x_tgt ** 2 + d7_y_tgt ** 2),
         d8_to_tgt = sqrt(d8_x_tgt ** 2 + d8_y_tgt ** 2),
         d9_to_tgt = sqrt(d9_x_tgt ** 2 + d9_y_tgt ** 2),
         d10_to_tgt = sqrt(d10_x_tgt ** 2 + d10_y_tgt ** 2),
         d11_to_tgt = sqrt(d11_x_tgt ** 2 + d11_y_tgt ** 2))


qbtd_mat <- as.matrix((qb_ball_target_defs) %>% select(gameId, playId, frameId, tgt_id, d1_to_tgt, d2_to_tgt, d3_to_tgt, d4_to_tgt, d5_to_tgt, d6_to_tgt, d7_to_tgt, d8_to_tgt, d9_to_tgt, d10_to_tgt, d11_to_tgt))

rr <- as.data.frame(rowRanks(qbtd_mat, cols = match(c("d1_to_tgt", "d2_to_tgt", "d3_to_tgt", "d4_to_tgt", "d5_to_tgt", "d6_to_tgt", "d7_to_tgt", "d8_to_tgt", "d9_to_tgt", "d10_to_tgt", "d11_to_tgt"), colnames(qbtd_mat))))
colnames(rr) <- paste0(colnames(rr), "_rank")


qbtd_ranks <- cbind(qb_ball_target_defs, rr) %>%
  mutate(db_1_x = case_when(d1_to_tgt_rank == 1 ~ d1_x,
                           d2_to_tgt_rank == 1 ~ d2_x,
                           d3_to_tgt_rank == 1 ~ d3_x,
                           d4_to_tgt_rank == 1 ~ d4_x,
                           d5_to_tgt_rank == 1 ~ d5_x,
                           d6_to_tgt_rank == 1 ~ d6_x,
                           d7_to_tgt_rank == 1 ~ d7_x,
                           d8_to_tgt_rank == 1 ~ d8_x,
                           d9_to_tgt_rank == 1 ~ d9_x,
                           d10_to_tgt_rank == 1 ~ d10_x,
                           d11_to_tgt_rank == 1 ~ d11_x),
         db_1_y = case_when(d1_to_tgt_rank == 1 ~ d1_y,
                           d2_to_tgt_rank == 1 ~ d2_y,
                           d3_to_tgt_rank == 1 ~ d3_y,
                           d4_to_tgt_rank == 1 ~ d4_y,
                           d5_to_tgt_rank == 1 ~ d5_y,
                           d6_to_tgt_rank == 1 ~ d6_y,
                           d7_to_tgt_rank == 1 ~ d7_y,
                           d8_to_tgt_rank == 1 ~ d8_y,
                           d9_to_tgt_rank == 1 ~ d9_y,
                           d10_to_tgt_rank == 1 ~ d10_y,
                           d11_to_tgt_rank == 1 ~ d11_y),
         db1_s = case_when(d1_to_tgt_rank == 1 ~ d1_s,
                           d2_to_tgt_rank == 1 ~ d2_s,
                           d3_to_tgt_rank == 1 ~ d3_s,
                           d4_to_tgt_rank == 1 ~ d4_s,
                           d5_to_tgt_rank == 1 ~ d5_s,
                           d6_to_tgt_rank == 1 ~ d6_s,
                           d7_to_tgt_rank == 1 ~ d7_s,
                           d8_to_tgt_rank == 1 ~ d8_s,
                           d9_to_tgt_rank == 1 ~ d9_s,
                           d10_to_tgt_rank == 1 ~ d10_s,
                           d11_to_tgt_rank == 1 ~ d11_s),
         db1_spd_x = case_when(d1_to_tgt_rank == 1 ~ d1_spd_x,
                               d2_to_tgt_rank == 1 ~ d2_spd_x,
                               d3_to_tgt_rank == 1 ~ d3_spd_x,
                               d4_to_tgt_rank == 1 ~ d4_spd_x,
                               d5_to_tgt_rank == 1 ~ d5_spd_x,
                               d6_to_tgt_rank == 1 ~ d6_spd_x,
                               d7_to_tgt_rank == 1 ~ d7_spd_x,
                               d8_to_tgt_rank == 1 ~ d8_spd_x,
                               d9_to_tgt_rank == 1 ~ d9_spd_x,
                               d10_to_tgt_rank == 1 ~ d10_spd_x,
                               d11_to_tgt_rank == 1 ~ d11_spd_x),
         db1_spd_y = case_when(d1_to_tgt_rank == 1 ~ d1_spd_y,
                               d2_to_tgt_rank == 1 ~ d2_spd_y,
                               d3_to_tgt_rank == 1 ~ d3_spd_y,
                               d4_to_tgt_rank == 1 ~ d4_spd_y,
                               d5_to_tgt_rank == 1 ~ d5_spd_y,
                               d6_to_tgt_rank == 1 ~ d6_spd_y,
                               d7_to_tgt_rank == 1 ~ d7_spd_y,
                               d8_to_tgt_rank == 1 ~ d8_spd_y,
                               d9_to_tgt_rank == 1 ~ d9_spd_y,
                               d10_to_tgt_rank == 1 ~ d10_spd_y,
                               d11_to_tgt_rank == 1 ~ d11_spd_y),
         db1_a_x = case_when(d1_to_tgt_rank == 1 ~ d1_a_x,
                               d2_to_tgt_rank == 1 ~ d2_a_x,
                               d3_to_tgt_rank == 1 ~ d3_a_x,
                               d4_to_tgt_rank == 1 ~ d4_a_x,
                               d5_to_tgt_rank == 1 ~ d5_a_x,
                               d6_to_tgt_rank == 1 ~ d6_a_x,
                               d7_to_tgt_rank == 1 ~ d7_a_x,
                               d8_to_tgt_rank == 1 ~ d8_a_x,
                               d9_to_tgt_rank == 1 ~ d9_a_x,
                               d10_to_tgt_rank == 1 ~ d10_a_x,
                               d11_to_tgt_rank == 1 ~ d11_a_x),
         db1_a_y = case_when(d1_to_tgt_rank == 1 ~ d1_a_y,
                               d2_to_tgt_rank == 1 ~ d2_a_y,
                               d3_to_tgt_rank == 1 ~ d3_a_y,
                               d4_to_tgt_rank == 1 ~ d4_a_y,
                               d5_to_tgt_rank == 1 ~ d5_a_y,
                               d6_to_tgt_rank == 1 ~ d6_a_y,
                               d7_to_tgt_rank == 1 ~ d7_a_y,
                               d8_to_tgt_rank == 1 ~ d8_a_y,
                               d9_to_tgt_rank == 1 ~ d9_a_y,
                               d10_to_tgt_rank == 1 ~ d10_a_y,
                               d11_to_tgt_rank == 1 ~ d11_a_y),
         db1_x_target = case_when(d1_to_tgt_rank == 1 ~ d1_x_tgt,
                                  d2_to_tgt_rank == 1 ~ d2_x_tgt,
                                  d3_to_tgt_rank == 1 ~ d3_x_tgt,
                                  d4_to_tgt_rank == 1 ~ d4_x_tgt,
                                  d5_to_tgt_rank == 1 ~ d5_x_tgt,
                                  d6_to_tgt_rank == 1 ~ d6_x_tgt,
                                  d7_to_tgt_rank == 1 ~ d7_x_tgt,
                                  d8_to_tgt_rank == 1 ~ d8_x_tgt,
                                  d9_to_tgt_rank == 1 ~ d9_x_tgt,
                                  d10_to_tgt_rank == 1 ~ d10_x_tgt,
                                  d11_to_tgt_rank == 1 ~ d11_x_tgt),
         distance_to_db1 = case_when(d1_to_tgt_rank == 1 ~ d1_to_tgt,
                                     d2_to_tgt_rank == 1 ~ d2_to_tgt,
                                     d3_to_tgt_rank == 1 ~ d3_to_tgt,
                                     d4_to_tgt_rank == 1 ~ d4_to_tgt,
                                     d5_to_tgt_rank == 1 ~ d5_to_tgt,
                                     d6_to_tgt_rank == 1 ~ d6_to_tgt,
                                     d7_to_tgt_rank == 1 ~ d7_to_tgt,
                                     d8_to_tgt_rank == 1 ~ d8_to_tgt,
                                     d9_to_tgt_rank == 1 ~ d9_to_tgt,
                                     d10_to_tgt_rank == 1 ~ d10_to_tgt,
                                     d11_to_tgt_rank == 1 ~ d11_to_tgt),
         db1_adj_dir = case_when(d1_to_tgt_rank == 1 ~ d1_adj_dir,
                               d2_to_tgt_rank == 1 ~ d2_adj_dir,
                               d3_to_tgt_rank == 1 ~ d3_adj_dir,
                               d4_to_tgt_rank == 1 ~ d4_adj_dir,
                               d5_to_tgt_rank == 1 ~ d5_adj_dir,
                               d6_to_tgt_rank == 1 ~ d6_adj_dir,
                               d7_to_tgt_rank == 1 ~ d7_adj_dir,
                               d8_to_tgt_rank == 1 ~ d8_adj_dir,
                               d9_to_tgt_rank == 1 ~ d9_adj_dir,
                               d10_to_tgt_rank == 1 ~ d10_adj_dir,
                               d11_to_tgt_rank == 1 ~ d11_adj_dir),
         db_2_x = case_when(d1_to_tgt_rank == 2 ~ d1_x,
                           d2_to_tgt_rank == 2 ~ d2_x,
                           d3_to_tgt_rank == 2 ~ d3_x,
                           d4_to_tgt_rank == 2 ~ d4_x,
                           d5_to_tgt_rank == 2 ~ d5_x,
                           d6_to_tgt_rank == 2 ~ d6_x,
                           d7_to_tgt_rank == 2 ~ d7_x,
                           d8_to_tgt_rank == 2 ~ d8_x,
                           d9_to_tgt_rank == 2 ~ d9_x,
                           d10_to_tgt_rank == 2 ~ d10_x,
                           d11_to_tgt_rank == 2 ~ d11_x),
         db_2_y = case_when(d1_to_tgt_rank == 2 ~ d1_y,
                           d2_to_tgt_rank == 2 ~ d2_y,
                           d3_to_tgt_rank == 2 ~ d3_y,
                           d4_to_tgt_rank == 2 ~ d4_y,
                           d5_to_tgt_rank == 2 ~ d5_y,
                           d6_to_tgt_rank == 2 ~ d6_y,
                           d7_to_tgt_rank == 2 ~ d7_y,
                           d8_to_tgt_rank == 2 ~ d8_y,
                           d9_to_tgt_rank == 2 ~ d9_y,
                           d10_to_tgt_rank == 2 ~ d10_y,
                           d11_to_tgt_rank == 2 ~ d11_y),
         db2_s = case_when(d1_to_tgt_rank == 2 ~ d1_s,
                           d2_to_tgt_rank == 2 ~ d2_s,
                           d3_to_tgt_rank == 2 ~ d3_s,
                           d4_to_tgt_rank == 2 ~ d4_s,
                           d5_to_tgt_rank == 2 ~ d5_s,
                           d6_to_tgt_rank == 2 ~ d6_s,
                           d7_to_tgt_rank == 2 ~ d7_s,
                           d8_to_tgt_rank == 2 ~ d8_s,
                           d9_to_tgt_rank == 2 ~ d9_s,
                           d10_to_tgt_rank == 2 ~ d10_s,
                           d11_to_tgt_rank == 2 ~ d11_s),
         db2_spd_x = case_when(d1_to_tgt_rank == 2 ~ d1_spd_x,
                               d2_to_tgt_rank == 2 ~ d2_spd_x,
                               d3_to_tgt_rank == 2 ~ d3_spd_x,
                               d4_to_tgt_rank == 2 ~ d4_spd_x,
                               d5_to_tgt_rank == 2 ~ d5_spd_x,
                               d6_to_tgt_rank == 2 ~ d6_spd_x,
                               d7_to_tgt_rank == 2 ~ d7_spd_x,
                               d8_to_tgt_rank == 2 ~ d8_spd_x,
                               d9_to_tgt_rank == 2 ~ d9_spd_x,
                               d10_to_tgt_rank == 2 ~ d10_spd_x,
                               d11_to_tgt_rank == 2 ~ d11_spd_x),
         db2_spd_y = case_when(d1_to_tgt_rank == 2 ~ d1_spd_y,
                               d2_to_tgt_rank == 2 ~ d2_spd_y,
                               d3_to_tgt_rank == 2 ~ d3_spd_y,
                               d4_to_tgt_rank == 2 ~ d4_spd_y,
                               d5_to_tgt_rank == 2 ~ d5_spd_y,
                               d6_to_tgt_rank == 2 ~ d6_spd_y,
                               d7_to_tgt_rank == 2 ~ d7_spd_y,
                               d8_to_tgt_rank == 2 ~ d8_spd_y,
                               d9_to_tgt_rank == 2 ~ d9_spd_y,
                               d10_to_tgt_rank == 2 ~ d10_spd_y,
                               d11_to_tgt_rank == 2 ~ d11_spd_y),
         db2_a_x = case_when(d1_to_tgt_rank == 2 ~ d1_a_x,
                             d2_to_tgt_rank == 2 ~ d2_a_x,
                             d3_to_tgt_rank == 2 ~ d3_a_x,
                             d4_to_tgt_rank == 2 ~ d4_a_x,
                             d5_to_tgt_rank == 2 ~ d5_a_x,
                             d6_to_tgt_rank == 2 ~ d6_a_x,
                             d7_to_tgt_rank == 2 ~ d7_a_x,
                             d8_to_tgt_rank == 2 ~ d8_a_x,
                             d9_to_tgt_rank == 2 ~ d9_a_x,
                             d10_to_tgt_rank == 2 ~ d10_a_x,
                             d11_to_tgt_rank == 2 ~ d11_a_x),
         db2_a_y = case_when(d1_to_tgt_rank == 2 ~ d1_a_y,
                             d2_to_tgt_rank == 2 ~ d2_a_y,
                             d3_to_tgt_rank == 2 ~ d3_a_y,
                             d4_to_tgt_rank == 2 ~ d4_a_y,
                             d5_to_tgt_rank == 2 ~ d5_a_y,
                             d6_to_tgt_rank == 2 ~ d6_a_y,
                             d7_to_tgt_rank == 2 ~ d7_a_y,
                             d8_to_tgt_rank == 2 ~ d8_a_y,
                             d9_to_tgt_rank == 2 ~ d9_a_y,
                             d10_to_tgt_rank == 2 ~ d10_a_y,
                             d11_to_tgt_rank == 2 ~ d11_a_y),
         db2_x_target = case_when(d1_to_tgt_rank == 2 ~ d1_x_tgt,
                                  d2_to_tgt_rank == 2 ~ d2_x_tgt,
                                  d3_to_tgt_rank == 2 ~ d3_x_tgt,
                                  d4_to_tgt_rank == 2 ~ d4_x_tgt,
                                  d5_to_tgt_rank == 2 ~ d5_x_tgt,
                                  d6_to_tgt_rank == 2 ~ d6_x_tgt,
                                  d7_to_tgt_rank == 2 ~ d7_x_tgt,
                                  d8_to_tgt_rank == 2 ~ d8_x_tgt,
                                  d9_to_tgt_rank == 2 ~ d9_x_tgt,
                                  d10_to_tgt_rank == 2 ~ d10_x_tgt,
                                  d11_to_tgt_rank == 2 ~ d11_x_tgt),
         distance_to_db2 = case_when(d1_to_tgt_rank == 2 ~ d1_to_tgt,
                                     d2_to_tgt_rank == 2 ~ d2_to_tgt,
                                     d3_to_tgt_rank == 2 ~ d3_to_tgt,
                                     d4_to_tgt_rank == 2 ~ d4_to_tgt,
                                     d5_to_tgt_rank == 2 ~ d5_to_tgt,
                                     d6_to_tgt_rank == 2 ~ d6_to_tgt,
                                     d7_to_tgt_rank == 2 ~ d7_to_tgt,
                                     d8_to_tgt_rank == 2 ~ d8_to_tgt,
                                     d9_to_tgt_rank == 2 ~ d9_to_tgt,
                                     d10_to_tgt_rank == 2 ~ d10_to_tgt,
                                     d11_to_tgt_rank == 2 ~ d11_to_tgt),
         db2_adj_dir = case_when(d1_to_tgt_rank == 2 ~ d1_adj_dir,
                                 d2_to_tgt_rank == 2 ~ d2_adj_dir,
                                 d3_to_tgt_rank == 2 ~ d3_adj_dir,
                                 d4_to_tgt_rank == 2 ~ d4_adj_dir,
                                 d5_to_tgt_rank == 2 ~ d5_adj_dir,
                                 d6_to_tgt_rank == 2 ~ d6_adj_dir,
                                 d7_to_tgt_rank == 2 ~ d7_adj_dir,
                                 d8_to_tgt_rank == 2 ~ d8_adj_dir,
                                 d9_to_tgt_rank == 2 ~ d9_adj_dir,
                                 d10_to_tgt_rank == 2 ~ d10_adj_dir,
                                 d11_to_tgt_rank == 2 ~ d11_adj_dir),
         db_3_x = case_when(d1_to_tgt_rank == 3 ~ d1_x,
                           d2_to_tgt_rank == 3 ~ d2_x,
                           d3_to_tgt_rank == 3 ~ d3_x,
                           d4_to_tgt_rank == 3 ~ d4_x,
                           d5_to_tgt_rank == 3 ~ d5_x,
                           d6_to_tgt_rank == 3 ~ d6_x,
                           d7_to_tgt_rank == 3 ~ d7_x,
                           d8_to_tgt_rank == 3 ~ d8_x,
                           d9_to_tgt_rank == 3 ~ d9_x,
                           d10_to_tgt_rank == 3 ~ d10_x,
                           d11_to_tgt_rank == 3 ~ d11_x),
         db_3_y = case_when(d1_to_tgt_rank == 3 ~ d1_y,
                           d2_to_tgt_rank == 3 ~ d2_y,
                           d3_to_tgt_rank == 3 ~ d3_y,
                           d4_to_tgt_rank == 3 ~ d4_y,
                           d5_to_tgt_rank == 3 ~ d5_y,
                           d6_to_tgt_rank == 3 ~ d6_y,
                           d7_to_tgt_rank == 3 ~ d7_y,
                           d8_to_tgt_rank == 3 ~ d8_y,
                           d9_to_tgt_rank == 3 ~ d9_y,
                           d10_to_tgt_rank == 3 ~ d10_y,
                           d11_to_tgt_rank == 3 ~ d11_y),
         db3_spd_x = case_when(d1_to_tgt_rank == 3 ~ d1_spd_x,
                               d2_to_tgt_rank == 3 ~ d2_spd_x,
                               d3_to_tgt_rank == 3 ~ d3_spd_x,
                               d4_to_tgt_rank == 3 ~ d4_spd_x,
                               d5_to_tgt_rank == 3 ~ d5_spd_x,
                               d6_to_tgt_rank == 3 ~ d6_spd_x,
                               d7_to_tgt_rank == 3 ~ d7_spd_x,
                               d8_to_tgt_rank == 3 ~ d8_spd_x,
                               d9_to_tgt_rank == 3 ~ d9_spd_x,
                               d10_to_tgt_rank == 3 ~ d10_spd_x,
                               d11_to_tgt_rank == 3 ~ d11_spd_x),
         db3_spd_y = case_when(d1_to_tgt_rank == 3 ~ d1_spd_y,
                               d2_to_tgt_rank == 3 ~ d2_spd_y,
                               d3_to_tgt_rank == 3 ~ d3_spd_y,
                               d4_to_tgt_rank == 3 ~ d4_spd_y,
                               d5_to_tgt_rank == 3 ~ d5_spd_y,
                               d6_to_tgt_rank == 3 ~ d6_spd_y,
                               d7_to_tgt_rank == 3 ~ d7_spd_y,
                               d8_to_tgt_rank == 3 ~ d8_spd_y,
                               d9_to_tgt_rank == 3 ~ d9_spd_y,
                               d10_to_tgt_rank == 3 ~ d10_spd_y,
                               d11_to_tgt_rank == 3 ~ d11_spd_y),
         db3_a_x = case_when(d1_to_tgt_rank == 3 ~ d1_a_x,
                             d2_to_tgt_rank == 3 ~ d2_a_x,
                             d3_to_tgt_rank == 3 ~ d3_a_x,
                             d4_to_tgt_rank == 3 ~ d4_a_x,
                             d5_to_tgt_rank == 3 ~ d5_a_x,
                             d6_to_tgt_rank == 3 ~ d6_a_x,
                             d7_to_tgt_rank == 3 ~ d7_a_x,
                             d8_to_tgt_rank == 3 ~ d8_a_x,
                             d9_to_tgt_rank == 3 ~ d9_a_x,
                             d10_to_tgt_rank == 3 ~ d10_a_x,
                             d11_to_tgt_rank == 3 ~ d11_a_x),
         db3_a_y = case_when(d1_to_tgt_rank == 3 ~ d1_a_y,
                             d2_to_tgt_rank == 3 ~ d2_a_y,
                             d3_to_tgt_rank == 3 ~ d3_a_y,
                             d4_to_tgt_rank == 3 ~ d4_a_y,
                             d5_to_tgt_rank == 3 ~ d5_a_y,
                             d6_to_tgt_rank == 3 ~ d6_a_y,
                             d7_to_tgt_rank == 3 ~ d7_a_y,
                             d8_to_tgt_rank == 3 ~ d8_a_y,
                             d9_to_tgt_rank == 3 ~ d9_a_y,
                             d10_to_tgt_rank == 3 ~ d10_a_y,
                             d11_to_tgt_rank == 3 ~ d11_a_y),
         db3_adj_dir = case_when(d1_to_tgt_rank == 3 ~ d1_adj_dir,
                                 d2_to_tgt_rank == 3 ~ d2_adj_dir,
                                 d3_to_tgt_rank == 3 ~ d3_adj_dir,
                                 d4_to_tgt_rank == 3 ~ d4_adj_dir,
                                 d5_to_tgt_rank == 3 ~ d5_adj_dir,
                                 d6_to_tgt_rank == 3 ~ d6_adj_dir,
                                 d7_to_tgt_rank == 3 ~ d7_adj_dir,
                                 d8_to_tgt_rank == 3 ~ d8_adj_dir,
                                 d9_to_tgt_rank == 3 ~ d9_adj_dir,
                                 d10_to_tgt_rank == 3 ~ d10_adj_dir,
                                 d11_to_tgt_rank == 3 ~ d11_adj_dir),
         db_2line = sqrt((db_1_x - db_2_x) ** 2 + (db_1_y - db_2_y) ** 2),
         db_triangle_area = .5 * abs((db_1_x * (db_2_y - db_3_y)) + (db_2_x * (db_3_y - db_1_y)) + (db_3_x * (db_1_y - db_2_y))),
         db_13 = sqrt((db_1_x - db_3_x) ** 2 + (db_1_y - db_3_y) ** 2),
         db_23 = sqrt((db_3_x - db_2_x) ** 2 + (db_3_y - db_2_y) ** 2),
         big_angle_db = case_when(db_2line >= db_13 & db_2line >= db_23 ~ 3,
                                  db_13 >= db_2line & db_13 >= db_23 ~ 2,
                                  db_23 >= db_2line & db_23 >= db_13 ~ 1),
         big_angle = case_when(big_angle_db == 3 ~ acos(((db_13 ** 2) + (db_23 ** 2) - (db_2line ** 2))/(db_2line * db_23 * 2)),
                               big_angle_db == 2 ~ acos(((db_2line ** 2) + (db_23 ** 2) - (db_13 ** 2))/(db_2line * db_23 * 2)),
                               big_angle_db == 1 ~ acos(((db_2line ** 2) + (db_13 ** 2) - (db_23 ** 2))/(db_13 * db_2line * 2))))

x_velo_model_insert <- qbtd_ranks %>%
  select(qb_x_target, qb_y_target, qb_to_target, qb_target_angle, target_adj_o, qb_tgt_x_rel_spd, qb_tgt_y_rel_spd,
         qb_hardest, qb_speed,
         target_spd_x, target_spd_y,
         tgt_to_sl, qb_to_sl,
         tgt_to_1d, tgt_to_gl, qb_to_los,
         db1_x_target, distance_to_db1, db2_x_target, distance_to_db2,
         db1_spd_x, db1_spd_y, db2_spd_x, db2_spd_y, db3_spd_x, db3_spd_y,
         db_2line, db_triangle_area, big_angle, big_angle_db,
         down, dropback_type, target_height, height_inches, route_throw,
         gameId, playId, frameId, tgt_id) %>%
  rename(target_to_sl = tgt_to_sl)

x_velo_model <- readRDS("C:/Users/Joshua/Desktop/BDB 2024-25/nfl-big-data-bowl-2025/Expected Throw Velo 1.rds")

x_velo_preds <- as.data.frame(
  matrix(predict(x_velo_model, as.matrix(x_velo_model_insert %>% select(-gameId, -playId, -frameId, -tgt_id))))) %>%
  dplyr::rename(x_throw_velo = V1) %>%
  bind_cols(qbtd_ranks) %>%
  filter(frameType == "AFTER_SNAP")

rm(tracking, track_plays, track_plays_players)

print(starter)

print(Sys.time())

start_throw <- x_velo_preds %>%
  mutate(d1_to_qb = sqrt((db_1_x - qb_x) ** 2 + (db_1_y - qb_y) ** 2),
         d2_to_qb = sqrt((db_2_x - qb_x) ** 2 + (db_2_y - qb_y) ** 2),
         d3_to_qb = sqrt((db_3_x - qb_x) ** 2 + (db_3_y - qb_y) ** 2),
         tgt_dir_ball = ifelse(target_adj_dir > qb_adj_dir, target_adj_dir - qb_adj_dir, 360 + target_adj_dir - qb_adj_dir),
         tgt_o_dir = ifelse(target_adj_o > target_adj_dir, target_adj_o - target_adj_dir, 360 + target_adj_o - target_adj_dir),
         target_adj_dir_rad = target_adj_dir * pi / 180,
         tgt_sindir = sin(target_adj_dir_rad),
         tgt_cosdir = cos(target_adj_dir_rad),
         quart_a = (target_a ** 2)/4,
         quart_b = target_s * target_a,
         quart_c = -(qb_x * target_a * tgt_sindir) + (tgt_x * target_a * tgt_sindir) + (qb_y * target_a * tgt_cosdir) - (tgt_y * target_a * tgt_cosdir) + (target_s ** 2) - (x_throw_velo ** 2),
         quart_d = -(2 * qb_x * target_s * tgt_sindir) + (2 * tgt_x * target_s * tgt_sindir) + (2 * qb_y * target_s * tgt_cosdir) - (2 * tgt_y * target_s * tgt_cosdir),
         quart_e = (qb_to_target ** 2))

start_throw <- start_throw %>%
  filter(frameId > snap_frame, !(gameId == 2018093006 & playId == 2573)) %>%
  rowwise() %>%
  mutate(roots = list(polyroot(c(quart_e, quart_d, quart_c, quart_b, quart_a))),
         small_root = min(Re(roots[which(Re(roots) > 0)]))) %>%
  ungroup()

start_throw2 <- start_throw %>%
  filter(!(is.infinite(small_root))) %>%
  rename(hang_time = small_root, qb_to_tgt_release = qb_to_target) %>%
  mutate(catch_point_x = tgt_x + (target_spd_x * hang_time) + (.5 * target_a * (sin(target_adj_dir_rad)) * (hang_time ** 2)),
         catch_point_y = tgt_y + (target_spd_y * hang_time) + (.5 * target_a * (-cos(target_adj_dir_rad)) * (hang_time ** 2)),
         throw_length = sqrt((qb_x - catch_point_x) ** 2 + (qb_y - catch_point_y) ** 2),
         cp_to_sl = ifelse(catch_point_y >= 160/6, 160/3-catch_point_y, catch_point_y),
         pass_x = abs(qb_x - catch_point_x),
         pass_y = abs(qb_y - catch_point_y),
         pass_angle_rad = atan(pass_x/pass_y),
         db1_xf = db_1_x + (db1_spd_x * hang_time) + (.5 * db1_a_x * (hang_time ** 2)),
         db1_yf = db_1_y + (db1_spd_y * hang_time) + (.5 * db1_a_y * (hang_time ** 2)),
         db2_xf = db_2_x + (db2_spd_x * hang_time) + (.5 * db2_a_x * (hang_time ** 2)),
         db2_yf = db_2_y + (db2_spd_y * hang_time) + (.5 * db2_a_y * (hang_time ** 2)),
         db3_xf = db_3_x + (db3_spd_x * hang_time) + (.5 * db3_a_x * (hang_time ** 2)),
         db3_yf = db_3_y + (db3_spd_y * hang_time) + (.5 * db3_a_y * (hang_time ** 2)),
         d1_to_catch = sqrt((db_1_x - catch_point_x) ** 2 + (db_1_y - catch_point_y) ** 2),
         d1_distance_covered_x = abs(db1_xf - db_1_x),
         d1_distance_covered_y = abs(db1_yf - db_1_y),
         d1_distance_covered = sqrt(d1_distance_covered_x ** 2 + d1_distance_covered_y ** 2),
         d1_final_x_cp = abs(db1_xf - catch_point_x),
         d1_final_y_cp = abs(db1_yf - catch_point_y),
         d1_final_from_cp = sqrt(d1_final_x_cp ** 2 + d1_final_y_cp ** 2),
         d1_final_y_sl = ifelse(db1_yf >= 160/6, 160/3-db1_yf, db1_yf),
         d1_dir_tgt = ifelse(d1_adj_dir > target_adj_dir, d1_adj_dir - target_adj_dir, 360 + d1_adj_dir - target_adj_dir),

         d2_to_catch = sqrt((db_2_x - catch_point_x) ** 2 + (db_2_y - catch_point_y) ** 2),
         d2_distance_covered_x = abs(db2_xf - db_2_x),
         d2_distance_covered_y = abs(db2_yf - db_2_y),
         d2_distance_covered = sqrt(d2_distance_covered_x ** 2 + d2_distance_covered_y ** 2),
         d2_final_x_cp = abs(db2_xf - catch_point_x),
         d2_final_y_cp = abs(db2_yf - catch_point_y),
         d2_final_from_cp = sqrt(d2_final_x_cp ** 2 + d2_final_y_cp ** 2),
         d2_final_y_sl = ifelse(db1_yf >= 160/6, 160/3-db1_yf, db1_yf),
         d2_dir_tgt = ifelse(d2_adj_dir > target_adj_dir, d2_adj_dir - target_adj_dir, 360 + d2_adj_dir - target_adj_dir),

         d3_to_catch = sqrt((db_3_x - catch_point_x) ** 2 + (db_3_y - catch_point_y) ** 2),
         d3_distance_covered_x = abs(db3_xf - db_3_x),
         d3_distance_covered_y = abs(db3_yf - db_3_y),
         d3_distance_covered = sqrt(d3_distance_covered_x ** 2 + d3_distance_covered_y ** 2),
         d3_final_x_cp = abs(db3_xf - catch_point_x),
         d3_final_y_cp = abs(db3_yf - catch_point_y),
         d3_final_from_cp = sqrt(d3_final_x_cp ** 2 + d3_final_y_cp ** 2),
         d3_final_y_sl = ifelse(db1_yf >= 160/6, 160/3-db1_yf, db1_yf),
         d3_dir_tgt = ifelse(d3_adj_dir > target_adj_dir, d3_adj_dir - target_adj_dir, 360 + d3_adj_dir - target_adj_dir),

         d1_i_from_d2 = sqrt((db_1_x - db_2_x) ** 2 + (db_1_y - db_2_y) ** 2),
         d1_f_from_d2 = sqrt((db1_xf - db2_xf) ** 2 + (db1_yf - db2_yf) ** 2),
         close_2_condense = abs(d1_f_from_d2 - d1_i_from_d2),
         
         tgt_cp_x = abs(tgt_x - catch_point_x),
         tgt_cp_y = abs(tgt_y - catch_point_y),
         tgt_to_cp = sqrt(tgt_cp_x ** 2 + tgt_cp_y ** 2))

rm(start_throw)
rm(pot_targets, qbtd_mat, rr, qbtd_ranks)


rm(defender_split_1, defender_split_10, defender_split_11, defender_split_2, defender_split_3, defender_split_4, defender_split_5, defender_split_6, defender_split_7, defender_split_8, defender_split_9, defenders1, merged_defenders, x_velo_model_insert, x_velo_preds, qbs, qb_ball)

the_targets <- data.table::fread("C:/Users/Joshua/Desktop/BDB 2024-25/nfl-big-data-bowl-2025/Expected Throw Velo Model 1 Results.csv") %>%
  select(gameId, playId, target_id, arrive_frame)

all_throws <- merge(start_throw2, the_targets, by = c("gameId", "playId"))

all_throws <- all_throws %>%
  mutate(pass_outcome = ifelse(passResult == "C", 0, ifelse(passResult == "I", 1, 2)),
         time_since_snap = as.numeric(frameId) - as.numeric(snap_frame),
         tgt_a_x = target_a * sin(target_adj_dir_rad),
         tgt_a_y = target_a * -cos(target_adj_dir_rad),
         cp_to_1d = ifelse(gl_x == 110, absolute_1d - catch_point_x, catch_point_x - absolute_1d),
         cp_to_gl = ifelse(playDirection == "left", catch_point_x - gl_x, gl_x - catch_point_x)) %>%
  arrange(gameId, playId, frameId) %>%
  select(-d4_a_x, -d4_a_y, -d4_adj_dir, -d4_id, -d4_spd_y, -d4_to_tgt, -d4_to_tgt_rank, -d4_x, -d4_y, -d4_y_tgt,
         -d5_a_x, -d5_a_y, -d5_adj_dir, -d5_id, -d5_spd_y, -d5_to_tgt, -d5_to_tgt_rank, -d5_x, -d5_y, -d5_y_tgt,
         -d6_a_x, -d6_a_y, -d6_adj_dir, -d6_id, -d6_spd_y, -d6_to_tgt, -d6_to_tgt_rank, -d6_x, -d6_y, -d6_y_tgt,
         -d7_a_x, -d7_a_y, -d7_adj_dir, -d7_id, -d7_spd_y, -d7_to_tgt, -d7_to_tgt_rank, -d7_x, -d7_y, -d7_y_tgt,
         -d8_a_x, -d8_a_y, -d8_adj_dir, -d8_id, -d8_spd_y, -d8_to_tgt, -d8_to_tgt_rank, -d8_x, -d8_y, -d8_y_tgt,
         -d9_a_x, -d9_a_y, -d9_adj_dir, -d9_id, -d9_spd_y, -d9_to_tgt, -d9_to_tgt_rank, -d9_x, -d9_y, -d9_y_tgt,
         -d10_a_x, -d10_a_y, -d10_adj_dir, -d10_id, -d10_spd_y, -d10_to_tgt, -d10_to_tgt_rank, -d10_x, -d10_y, -d10_y_tgt,
         -d11_a_x, -d11_a_y, -d11_adj_dir, -d11_id, -d11_spd_y, -d11_to_tgt, -d11_to_tgt_rank, -d11_x, -d11_y, -d11_y_tgt)