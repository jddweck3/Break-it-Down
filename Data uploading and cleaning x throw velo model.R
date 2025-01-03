library(tidyverse)

#Pass in the data
data_spot <- "C:/Users/Joshua/Desktop/BDB 2024-25/nfl-big-data-bowl-2025/"
tracking <- data.table::fread(paste0(data_spot, "tracking_week_1.csv"))
for(i in 2:9){
  tracking <- rbind(tracking, data.table::fread(paste0(data_spot, "tracking_week_", i, ".csv")))
}
# write_csv(tracking, "C:/Users/Joshua/Desktop/BDB 2024-25/nfl-big-data-bowl-2025/Combined Tracking Data.csv")
#Map orientation and direction to the unit circle and orient the x values to the goal line
tracking <- tracking %>%
  mutate(gameplay = paste(gameId, playId),
         dir = ifelse(dir == 360, 0, dir),
         adj_dir = round(180 + (360 * floor(dir/180)) - dir, 2),
         adj_dir_rad = adj_dir * pi / 180,
         #The next line converts 360 degree orientations to 0. Since the orientation goes in a circle, they are the same.
         o = ifelse(o == 360, 0, o),
         adj_o = round(180 + (360 * floor(o/180)) - o, 2),
         adj_o_rad = adj_o * pi / 180,
         gl_x = ifelse(playDirection == "left", 10, 110),
         x_to_gl = ifelse(playDirection == "left", x - gl_x, gl_x - x))


players <- read_csv(paste0(data_spot, "players.csv"))
in_heights <- c()

#Convert player heights to inches
for(i in players$height){
  if(grepl("-", i)){
    in_heights <- c(in_heights, as.numeric(strsplit(i, "-")[[1]][1]) * 12 + as.numeric(strsplit(i, "-")[[1]][2]))
  } else{
    in_heights <- c(in_heights, as.numeric(i))
  }
}
player_heights <- players %>%
  mutate(height_inches = in_heights) %>%
  select(nflId, height_inches)

#Pass in plays and transform dropback type into a dummy variable
plays <- read_csv(paste0(data_spot, "plays.csv")) %>%
  mutate(dropback_type = case_when(dropbackType == "DESIGNED_ROLLOUT_LEFT" ~ 0,
                                   dropbackType == "DESIGNED_ROLLOUT_RIGHT" ~ 1,
                                   dropbackType == "SCRAMBLE" ~ 2,
                                   dropbackType == "SCRAMBLE_ROLLOUT_LEFT" ~ 3,
                                   dropbackType == "SCRAMBLE_ROLLOUT_RIGHT" ~ 4,
                                   dropbackType == "TRADITIONAL" ~ 5,
                                   T ~ 6))


#Get where the ball is at the time of the throw
throw_time <- tracking %>%
  filter(club %in% c("football", "Football"), event == "pass_forward") %>%
  select(gameId, playId, frameId) %>%
  rename(throw_frame = frameId)

#Get where the ball is at the time of the snap
snap_time <- tracking %>%
  filter(club %in% c("football", "Football"), frameType == "SNAP") %>%
  select(gameId, playId, frameId) %>%
  rename(snap_frame = frameId)

#Get where the ball is at the time of the pass's arrival
arrive_time <- tracking %>%
  filter(club %in% c("football", "Football"), event == "pass_arrived") %>%
  select(gameId, playId, frameId) %>%
  rename(arrive_frame = frameId) %>%
  mutate(gameplay = paste(gameId, playId))

#Get where the ball is at the time of an incompletion where there is no pass arrival
inc_time <- tracking %>%
  filter(club %in% c("football", "Football"), !(gameplay %in% unique(arrive_time$gameplay)), event %in% c("pass_outcome_incomplete", "out_of_bounds")) %>%
  select(gameId, playId, frameId) %>%
  rename(arrive_frame = frameId) %>%
  mutate(gameplay = paste(gameId, playId))

#Get where the ball is at the time of an interception
int_time <- tracking %>%
  filter(club %in% c("football", "Football"), !(gameplay %in% c(unique(arrive_time$gameplay), unique(inc_time$gameplay))), event == "pass_outcome_interception") %>%
  select(gameId, playId, frameId) %>%
  rename(arrive_frame = frameId) %>%
  mutate(gameplay = paste(gameId, playId))

#Get where the ball is at the time of a reception
catch_time <- tracking %>%
  filter(club %in% c("football", "Football"), !(gameplay %in% c(unique(arrive_time$gameplay), unique(inc_time$gameplay), unique(int_time$gameplay))), event == "pass_outcome_caught") %>%
  select(gameId, playId, frameId) %>%
  rename(arrive_frame = frameId) %>%
  mutate(gameplay = paste(gameId, playId))

#Get where the ball is at the time of a touchdown (when the event is pass_outcome_touchdown instead of any of the others)
td_time <- tracking %>%
  filter(club %in% c("football", "Football"), !(gameplay %in% c(unique(arrive_time$gameplay), unique(inc_time$gameplay), unique(int_time$gameplay), unique(catch_time$gameplay))), event == "pass_outcome_touchdown") %>%
  select(gameId, playId, frameId) %>%
  rename(arrive_frame = frameId) %>%
  mutate(gameplay = paste(gameId, playId))

#We start by joining together the ball tracking data at the time of the snap and the throw
track_throw <- tracking %>%
  merge(throw_time, by = c("gameId", "playId")) %>%
  merge(snap_time, by = c("gameId", "playId"))

rm(throw_time, snap_time)

#Merge in who the players are for each frame - this is mainly for team and position identification
track_throw <- left_join(track_throw, players %>% select(nflId, position), by = c("nflId")) %>%
  mutate(is_ball = ifelse(club %in% c("football", "Football"), 1, 0)) %>%
  arrange(gameId, playId, frameId, is_ball) %>%
  group_by(gameId, playId, frameId) %>%
  mutate(ball_speed = last(s))

rm(in_heights, players)

#Isolate the QBs and merge in their heights
qb <- track_throw %>%
  filter(position == "QB") %>%
  merge(player_heights, by = c("nflId")) %>%
  arrange(gameId, playId, frameId, is_ball)

#Calculate time from snap to pass
snap_to_pass <- qb %>%
  group_by(gameId, playId) %>%
  summarize(snap_to_pass = last(throw_frame) - last(snap_frame))

#Get max ball speed for each play - this will be the throw velocity out of the hand
qb_ps <- qb %>%
  filter(frameId >= snap_frame + min(snap_to_pass$snap_to_pass)) %>%
  group_by(gameId, playId) %>%
  mutate(max_ball_speed = max(ball_speed))

#Plot ball speed through the play
qb %>%
  filter(gameId == 2022091200) %>%
  arrange(playId) %>%
  ggplot(aes(x = frameId, y = ball_speed, color = as.character(playId))) +
  geom_line()

qb_ps %>%
  filter(gameId == 2022091200) %>%
  arrange(playId) %>%
  ggplot(aes(x = frameId, y = ball_speed, color = as.character(playId))) +
  geom_line()

#Isolate just the time of the pass and get each QB's hardest throw in the data
qb_at_pass <- qb_ps %>%
  filter(event == "pass_forward") %>%
  group_by(nflId) %>%
  mutate(qb_hardest = max(max_ball_speed))
rm(qb_ps)

#Isolate everyone but the QB at the time of the pass
at_pass <- track_throw %>%
  filter(event == "pass_forward", position != "QB")

#Pass in the games and player_play datasets
games <- read_csv(paste0(data_spot, "games.csv"))
player_play <- read_csv(paste0(data_spot, "player_play.csv"))

#Isolate just the players who ran a route at the time of the throw and convert their route to a dummy variable
targets_at_pass <- at_pass %>%
  left_join(player_play %>% select(gameId, playId, nflId, routeRan), by = c("gameId", "playId", "nflId")) %>%
  filter(!(is.na(routeRan))) %>%
  mutate(route_dummy = case_when(routeRan == "ANGLE" ~ 0,
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
                                 T ~ 13))

#Combine all of the pass arrival datasets into one
app_passes <- rbind(arrive_time, inc_time, int_time, catch_time, td_time) %>%
  arrange(gameId, playId, arrive_frame) %>%
  select(-gameplay)

rm(arrive_time, inc_time, int_time, catch_time, td_time)

#Merge arrival time data into the throw time data
track_targets <- track_throw %>%
  merge(app_passes, by = c("gameId", "playId")) %>%
  distinct(gameId, playId, frameId, nflId, .keep_all = TRUE)

#Isolate just the players who ran a route at the time of pass arrival and convert their route to a dummy variable
targets_at_arrive <- track_targets %>%
  left_join(player_play %>% select(gameId, playId, nflId, routeRan), by = c("gameId", "playId", "nflId")) %>%
  filter(!(is.na(routeRan)) | is_ball == 1) %>%
  filter(frameId == arrive_frame) %>%
  mutate(route_dummy = case_when(routeRan == "ANGLE" ~ 0,
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
                                 T ~ 13))

targets_at_pass <- targets_at_pass %>%
  mutate(full_combo = paste(gameId, playId, nflId))
targets_at_arrive <- targets_at_arrive %>%
  mutate(full_combo = paste(gameId, playId, nflId))

#Get where the ball is at the time of arrival
ball_at_arrive <- targets_at_arrive %>%
  filter(is_ball == 1) %>%
  select(gameId, playId, x, y) %>%
  rename(ball_x = x, ball_y = y)

#Figure out where everyone is relative to the ball at pass arrival time - closest is the target
at_arrive <- merge(targets_at_arrive, ball_at_arrive, by = c("gameId", "playId")) %>%
  filter(is_ball == 0) %>%
  mutate(distance_to_ball = sqrt((x - ball_x) ** 2 + (y - ball_y) ** 2)) %>%
  arrange(distance_to_ball) %>%
  group_by(gameId, playId) %>%
  mutate(distance_rank = row_number()) %>%
  arrange(gameId, playId, distance_rank)

#Isolate the targets only
the_target <- at_arrive %>%
  filter(distance_rank == 1) %>%
  # rename(target_id = nflId) %>%
  select(gameId, playId, nflId, arrive_frame) %>%
  merge(targets_at_pass, by = c("gameId", "playId", "nflId"))

gamer <- games %>%
  select(gameId, homeTeamAbbr, visitorTeamAbbr)

player <- plays %>%
  select(gameId, playId, down, yardsToGo, absoluteYardlineNumber, dropback_type, possessionTeam)

game_plays <- merge(gamer, player, by = c("gameId")) %>%
  mutate(is_offense_home = ifelse(possessionTeam == homeTeamAbbr, 1, 0))

#Merge the game and play event data into the target data 
the_target <- the_target %>%
  merge(game_plays, by = c("gameId", "playId")) %>%
  rename(target_id = nflId,
         target_x = x,
         target_y = y,
         target_s = s,
         # target_a = a,
         target_dis = dis,
         target_o = o,
         target_adj_o = adj_o,
         target_adj_o_rad = adj_o_rad,
         target_dir = dir,
         target_adj_dir = adj_dir,
         target_adj_dir_rad = adj_dir_rad,
         target_name = displayName,
         target_num = jerseyNumber,
         route_throw = route_dummy) %>%
  group_by(gameId, playId, target_id) %>%
  arrange(target_id, gameId, playId, frameId) %>%
  mutate(target_a = (target_s - lag(target_s)) * 10) %>%
  ungroup() %>%
  arrange(gameId, playId, frameId, target_id) %>%
  select(target_id, target_x, target_y, target_s, target_a, target_o, target_adj_o, target_adj_o_rad, target_dir, target_adj_dir, target_adj_dir_rad, target_dis, target_name, target_num, route_throw, gameId, playId, possessionTeam, homeTeamAbbr, visitorTeamAbbr, down, frameId, arrive_frame) %>%
  merge(player_heights, by.x = c("target_id"), by.y = c("nflId")) %>%
  rename(target_height = height_inches,
         release_frame = frameId) %>%
  distinct(gameId, playId, .keep_all = TRUE)

#Merge game and play event data into non-qb pass time data
at_pass_1 <- at_pass %>%
  merge(game_plays, by = c("gameId", "playId")) %>%
  mutate(is_home = ifelse(club == homeTeamAbbr, 1, 0),
         is_offense_home = ifelse(possessionTeam == homeTeamAbbr, 1, 0),
         is_offense = ifelse(is_home == is_offense_home, 1, 0),
         absolute_1d = ifelse(gl_x == 10, as.numeric(absoluteYardlineNumber) - as.numeric(yardsToGo), as.numeric(absoluteYardlineNumber) + as.numeric(yardsToGo))) %>%
  distinct(gameId, playId, frameId, nflId, .keep_all = TRUE)

#Get where each defender is relative to the target at the time of the pass
def_at_pass <- at_pass_1 %>%
  filter(is_offense == 0) %>%
  merge(the_target, by = c("gameId", "playId", "possessionTeam", "homeTeamAbbr", "visitorTeamAbbr")) %>%
  mutate(distance_to_target = sqrt((x - target_x) ** 2 + (y - target_y) ** 2)) %>%
  arrange(distance_to_target) %>%
  group_by(gameId, playId) %>%
  mutate(distance_rank = row_number()) %>%
  arrange(gameId, playId, distance_rank) %>%
  distinct(gameId, playId, frameId, nflId, .keep_all = TRUE)

#Define closest defender to target
closest_def <- def_at_pass %>%
  filter(distance_rank == 1) %>%
  rename(db_1_x = x,
         db_1_y = y,
         db1_id = nflId,
         db1_num = jerseyNumber) %>%
  mutate(distance_to_db1 = distance_to_target,
         db1_x_target = abs(db_1_x - target_x),
         db1_spd_x = s * sin(adj_dir_rad),
         db1_spd_y = s * -cos(adj_dir_rad)) %>%
  select(gameId, playId, db1_id, db_1_x, db_1_y, db1_spd_x, db1_spd_y, distance_to_db1, db1_x_target, db1_num, absolute_1d, absoluteYardlineNumber, dropback_type)

#Define 2nd closest defender to target
def_2 <- def_at_pass %>%
  filter(distance_rank == 2) %>%
  rename(db_2_x = x,
         db_2_y = y,
         db2_id = nflId,
         db2_num = jerseyNumber) %>%
  mutate(distance_to_db2 = distance_to_target,
         db2_x_target = abs(db_2_x - target_x),
         db2_spd_x = s * sin(adj_dir_rad),
         db2_spd_y = s * -cos(adj_dir_rad)) %>%
  select(gameId, playId, db2_id, db_2_x, db_2_y, db2_spd_x, db2_spd_y, distance_to_db2, db2_x_target, db2_num)

#Define 3rd closest defender to target
def_3 <- def_at_pass %>%
  filter(distance_rank == 3) %>%
  rename(db_3_x = x,
         db_3_y = y,
         db3_id = nflId,
         db3_num = jerseyNumber) %>%
  mutate(db3_spd_x = s * sin(adj_dir_rad),
         db3_spd_y = s * -cos(adj_dir_rad)) %>%
  select(gameId, playId, db3_id, db_3_x, db_3_y, db3_spd_x, db3_spd_y, db3_num)

#Merge the 3 closest defenders to the distance to target together and calculate their geometry
defenders <- merge(closest_def, def_2, by = c("gameId", "playId")) %>%
  merge(def_3, by = c("gameId", "playId")) %>%
  mutate(db_2line = sqrt((db_1_x - db_2_x) ** 2 + (db_1_y - db_2_y) ** 2),
         db_triangle_area = .5 * abs((db_1_x * (db_2_y - db_3_y)) + (db_2_x * (db_3_y - db_1_y)) + (db_3_x * (db_1_y - db_2_y))),
         db_13 = sqrt((db_1_x - db_3_x) ** 2 + (db_1_y - db_3_y) ** 2),
         db_23 = sqrt((db_3_x - db_2_x) ** 2 + (db_3_y - db_2_y) ** 2),
         big_angle_db = case_when(db_2line >= db_13 & db_2line >= db_23 ~ 3,
                                  db_13 >= db_2line & db_13 >= db_23 ~ 2,
                                  db_23 >= db_2line & db_23 >= db_13 ~ 1),
         big_angle = case_when(big_angle_db == 3 ~ acos(((db_13 ** 2) + (db_23 ** 2) - (db_2line ** 2))/(db_2line * db_23 * 2)),
                               big_angle_db == 2 ~ acos(((db_2line ** 2) + (db_23 ** 2) - (db_13 ** 2))/(db_2line * db_23 * 2)),
                               big_angle_db == 1 ~ acos(((db_2line ** 2) + (db_13 ** 2) - (db_23 ** 2))/(db_13 * db_2line * 2)))) %>%
  distinct()

#Combine the QB and the target
qb_target <- merge(qb_at_pass, the_target, by = c("gameId", "playId")) %>%
  mutate(qb_x_target = abs(x - target_x),
         qb_y_target = abs(y - target_y),
         qb_to_target = sqrt(qb_x_target ** 2 + qb_y_target ** 2),
         qb_target_angle = atan(qb_y_target/qb_x_target),
         qb_spd_x = s * sin(adj_dir_rad),
         qb_spd_y = s * -cos(adj_dir_rad),
         qb_to_sl = ifelse(y >= 160/6, 160/3-y, y),
         target_spd_x = target_s * sin(target_adj_dir_rad),
         target_spd_y = target_s * -cos(target_adj_dir_rad),
         target_to_sl = ifelse(target_y >= 160/6, 160/3-target_y, target_y),
         qb_tgt_x_rel_spd = qb_spd_x - target_spd_x,
         qb_tgt_y_rel_spd = qb_spd_y - target_spd_y,
         tgt_to_gl = ifelse(gl_x == 110, gl_x - target_x, target_x - gl_x))
qb_targeter <- unique(qb_target)
qb_target <- qb_targeter %>%
  distinct(gameId, playId, .keep_all = TRUE)

#This is a check to make sure that you don't have multiple QBs or multiple throws in the same play
failures <- qb_targeter %>% group_by(gameId, playId) %>% summarize(frames = n()) %>% filter(frames > 1) %>% mutate(gameplay = paste(gameId, playId))

#Combine the offensive players with the defensive players
qb_target_def <- merge(qb_target, defenders, by = c("gameId", "playId")) %>%
  distinct(gameId, playId, .keep_all = TRUE) %>%
  mutate(tgt_to_1d = ifelse(gl_x == 110, absolute_1d - target_x, target_x - absolute_1d),
         qb_to_los = -(ifelse(playDirection == "right", x - absoluteYardlineNumber, absoluteYardlineNumber - x)))
