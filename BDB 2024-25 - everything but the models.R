library(tidyverse)
library(gt)
library(gtExtras)
library(reshape2)
library(gridExtra)
library(gganimate)
library(av)

#Import tracking data
track <- as.data.frame(data.table::fread("C:/Users/Joshua/Desktop/BDB 2024-25/nfl-big-data-bowl-2025/tracking_week_1.csv"))
for(i in 2:9){
  track <- rbind(track, as.data.frame(data.table::fread(paste0("C:/Users/Joshua/Desktop/BDB 2024-25/nfl-big-data-bowl-2025/tracking_week_", as.character(i), ".csv"))))
}

track$jerseyNumber[is.na(track$jerseyNumber)] <- "B"

#Adjust orientation and direction so they fit on the unit circle and orient players relative to the goal line
track <- track %>%
  select(-time) %>%
  mutate(x_ez = ifelse(playDirection == "left", 10, 110),
         yds_ez = abs(x_ez - x),
         dir = ifelse(dir == 360, 0, dir),
         adj_dir = round(180 + (360 * floor(dir/180)) - dir, 2),
         adj_dir_rad = adj_dir * pi / 180,
         o = ifelse(o == 360, 0, o),
         adj_o = round(180 + (360 * floor(o/180)) - o, 2),
         adj_o_rad = adj_o * pi / 180)

#Import the rest of the data
plays <- as.data.frame(data.table::fread("C:/Users/Joshua/Desktop/BDB 2024-25/nfl-big-data-bowl-2025/plays.csv"))
players <- as.data.frame(data.table::fread("C:/Users/Joshua/Desktop/BDB 2024-25/nfl-big-data-bowl-2025/players.csv"))
player_plays <- as.data.frame(data.table::fread("C:/Users/Joshua/Desktop/BDB 2024-25/nfl-big-data-bowl-2025/player_play.csv"))
games <- as.data.frame(data.table::fread("C:/Users/Joshua/Desktop/BDB 2024-25/nfl-big-data-bowl-2025/games.csv"))

#Get descriptive elements of the play (gameId, playId, quarter, down, distance, etc.)
play_desc <- plays %>%
  select(gameId, playId, playDescription, quarter, down, yardsToGo, possessionTeam, defensiveTeam, absoluteYardlineNumber, isDropback)

#Separate the tracking data into pre-snap, snap, and post-snap
pre_snaps <- track %>%
  filter(frameType == "BEFORE_SNAP") %>%
  merge(play_desc, by = c("gameId", "playId")) %>%
  mutate(x_1d = ifelse(playDirection == "left", absoluteYardlineNumber - yardsToGo, absoluteYardlineNumber + yardsToGo))
snaps <- track %>%
  filter(frameType == "SNAP") %>%
  merge(play_desc, by = c("gameId", "playId")) %>%
  mutate(x_1d = ifelse(playDirection == "left", absoluteYardlineNumber - yardsToGo, absoluteYardlineNumber + yardsToGo))
post_snaps <- track %>%
  filter(frameType == "AFTER_SNAP") %>%
  merge(play_desc, by = c("gameId", "playId")) %>%
  mutate(x_1d = ifelse(playDirection == "left", absoluteYardlineNumber - yardsToGo, absoluteYardlineNumber + yardsToGo))

rm(track)
gc()

#Separate pre-snap tracking data into offense and defense (the ball counts as both)
pre_snap_o <- pre_snaps %>%
  filter(club == possessionTeam | club == "football")
pre_snap_d <- pre_snaps %>%
  filter(club == defensiveTeam | club == "football")
rm(pre_snaps)
gc()

#Get which players are in motion at the snap on which plays
identify_motion <- player_plays %>%
  select(gameId, playId, nflId, inMotionAtBallSnap, shiftSinceLineset, motionSinceLineset, wasRunningRoute, routeRan) %>%
  merge(players, by = c("nflId")) %>%
  filter(position %in% c("RB", "FB", "TE", "WR"), wasRunningRoute == 1) %>%
  mutate(motion_at_snap = ifelse(inMotionAtBallSnap == T, 1, 0),
         motion_at_snap = replace(motion_at_snap, is.na(motion_at_snap), 0))
table(identify_motion$motion_at_snap)

#Separate out the offense and the ball of the post-snap tracking data
post_snap_o <- post_snaps %>%
  filter(club == possessionTeam | club == "football")

#Separate the data further, isolating the passing plays and merging them with the motions
#This will give us which players are in motion at the snap on which passing plays
post_snap_o_pass <- post_snap_o %>%
  filter(isDropback == T) %>%
  mutate(gameplay = paste(gameId, playId)) %>%
  select(-displayName) %>%
  merge(identify_motion, by = c("gameId", "playId", "nflId"))
  
unique(post_snap_o_pass$motion_at_snap)

#Separate out the defense and the ball of the post-snap tracking data
post_snap_d <- post_snaps %>%
  filter(club != possessionTeam)

#Identify where the breaks are
identify_breaks <- post_snap_o_pass %>%
  arrange(nflId, gameId, playId, frameId) %>%
  mutate(dir_in_4 = lead(adj_dir, 4)) %>%
  rowwise() %>%
  mutate(dir_diff = min(abs(dir_in_4 - adj_dir), abs(adj_dir - dir_in_4), 360 - (dir_in_4 - adj_dir), 360 - (adj_dir - dir_in_4))) %>%
  ungroup() %>%
  group_by(nflId, gameId, playId) %>%
  filter(frameId < (max(frameId) - 4), frameId > (min(frameId) + 4)) %>%
  filter(frameId < (min(frameId) + 35)) %>%
  arrange(frameId) %>%
  mutate(cut_angle = max(dir_diff),
         # cut_frames = which.max(dir_diff),
         cut_time = frameId[which.max(dir_diff)] + 2) %>%
  ungroup()

#Graph the distribution of cut angles by position
identify_breaks %>%
  ggplot() +
  geom_density(aes(x = cut_angle)) +
  labs(x = "Angle of Cut",
       title = "Distribution of Angles of Cuts in Routes") +
  facet_wrap(~position)

# identify_breaks %>%
#   ggplot() +
#   geom_density(aes(x = cut_angle)) +
#   labs(x = "Angle of Cut",
#        title = "Distribution of Angles of Cuts in Routes") +
#   facet_wrap(~position)

#For each play, get the player's cut angle and whether they were in motion at the snap
route_diffs <- identify_breaks %>%
  group_by(nflId, gameId, playId, motion_at_snap, routeRan, cut_angle) %>%
  summarize(motion_at_snap = as.factor(first(motion_at_snap))) %>%
  ungroup()

# View(head(route_diffs, 5))

#Graph distribution of cut angles by route type
route_diffs %>%
  ggplot() +
  geom_density(aes(x = cut_angle, fill = motion_at_snap)) +
  labs(x = "Angle of Cut",
       title = "Distribution of Angles of Cuts in Routes") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1)) +
  facet_wrap(~routeRan)

#T test for cut angles
route_diffs_t <- route_diffs %>%
  # filter(frames_into_break >= 5) %>%
  group_by(routeRan) %>%
  summarize(alternative_hypo = ifelse(last(routeRan) %in% c("POST", "SLANT", "IN", "CORNER", "GO", "CROSS", "OUT", "WHEEL", "HITCH"), "less", "greater"),
            t_stat = t.test(cut_angle[motion_at_snap == 0], 
                            cut_angle[motion_at_snap == 1], 
                            alternative = alternative_hypo, 
                            var.equal = FALSE)$statistic,
            p_value = t.test(cut_angle[motion_at_snap == 0], 
                             cut_angle[motion_at_snap == 1], 
                             alternative = alternative_hypo, 
                             var.equal = FALSE)$p.value,
            times_run = n(),
            mean_cut_angle_motion = mean(cut_angle[motion_at_snap == 1]),
            mean_cut_angle_no_motion = mean(cut_angle[motion_at_snap == 0])) %>%
  mutate(alternative_hypo = paste("no motion", alternative_hypo)) %>%
  rename(cut_t_stat = t_stat,
         cut_p_value = p_value,
         cut_direction = alternative_hypo) %>%
  mutate(cut_significant = ifelse(cut_p_value < .1, 1, 0))
# route_diffs_t

#Set up what we need to calculate cut quality
cut_quality_setup <- identify_breaks %>%
  group_by(gameId, playId, nflId) %>%
  arrange(frameId) %>%
  mutate(last_x = lag(x),
         last_y = lag(y),
         distance_covered = sqrt((y - lag(y)) ** 2 + (x - lag(x)) ** 2),
         distance_covered = ifelse(is.na(distance_covered), 0, distance_covered),
         total_distance = cumsum(distance_covered),
         spd_x = s * sin(adj_dir_rad),
         spd_y = s * -cos(adj_dir_rad),
         spd_x = ifelse(x_ez == 110, spd_x, -spd_x)) %>%
  ungroup()

#For each player on each passing play, get their kinematics and cut quality at BREAK ENTRANCE
before_cut <- cut_quality_setup %>%
  group_by(nflId, gameId, playId) %>%
  filter(frameId <= cut_time) %>%
  mutate(distance_to_cut = max(total_distance)) %>%
  ungroup() %>%
  filter(distance_to_cut - total_distance <= 4) %>%
  group_by(nflId, gameId, playId, routeRan, motion_at_snap) %>%
  summarize(frames_into_break = -min(frameId) + last(cut_time),
            cut_time = last(cut_time),
            in_distance = last(distance_to_cut) - total_distance[which.min(frameId)],
            cut_place = last(distance_to_cut),
            cut_spd_x = last(spd_x),
            cut_spd_y = last(spd_y),
            cut_spd = last(s),
            enter_spd_x = spd_x[which.min(frameId)],
            enter_spd_y = spd_y[which.min(frameId)],
            enter_spd = s[which.min(frameId)],
            #THIS IS IN YARDS PER SECOND SQUARED
            in_accel_x = (cut_spd_x - enter_spd_x)/((last(frameId) - min(frameId))/10),
            in_accel_y = (cut_spd_y - enter_spd_y)/((last(frameId) - min(frameId))/10),
            in_accel = (cut_spd - enter_spd)/((last(frameId) - min(frameId))/10)) %>%
  ungroup() %>%
  merge(players, by = c("nflId")) %>%
  mutate(in_force_x = in_accel_x * weight * 0.453592,
         in_force = in_accel * weight * 0.453592,
         in_impulse_x = in_force_x * (frames_into_break / 10),
         in_impulse = in_force * (frames_into_break / 10)) %>%
  arrange(gameId, playId, nflId)
View(before_cut)

#For each player on each passing play, get their kinematics and cut quality at BREAK EXIT
after_cut <- cut_quality_setup %>%
  group_by(nflId, gameId, playId) %>%
  filter(frameId >= cut_time) %>%
  mutate(distance_to_cut = min(total_distance)) %>%
  ungroup() %>%
  filter(total_distance - distance_to_cut <= 2) %>%
  group_by(nflId, gameId, playId, routeRan, motion_at_snap) %>%
  summarize(frames_out_break = max(frameId) - last(cut_time),
            cut_time = last(cut_time),
            out_distance = -distance_to_cut[which.max(frameId)] + total_distance[which.max(frameId)],
            exit_spd_x = last(spd_x),
            exit_spd_y = last(spd_y),
            exit_spd = last(s),
            cut_spd_x = spd_x[which.min(frameId)],
            cut_spd_y = spd_y[which.min(frameId)],
            cut_spd = s[which.min(frameId)],
            #THIS IS IN YARDS PER SECOND SQUARED
            out_accel_x = (exit_spd_x - cut_spd_x)/((last(frameId) - min(frameId))/10),
            out_accel_y = (exit_spd_y - cut_spd_y)/((last(frameId) - min(frameId))/10),
            out_accel = (exit_spd - cut_spd)/((last(frameId) - min(frameId))/10),
            exit_time = max(frameId)) %>%
  ungroup() %>%
  merge(players, by = c("nflId")) %>%
  mutate(out_force_x = out_accel_x * weight * 0.453592,
         out_force = out_accel * weight * 0.453592,
         out_impulse_x = out_force_x * (frames_out_break / 10),
         out_impulse = out_force * (frames_out_break / 10)) %>%
  arrange(gameId, playId, nflId)
View(after_cut)

# write_csv(after_cut %>% select(gameId, playId, nflId, cut_time, exit_time), "Cut times w exits.csv")

#Graph distributions of break entrance quality by route
before_cut %>%
  filter(!(routeRan %in% c("CROSS", "GO", "SCREEN")), in_distance > 0) %>%
  # filter(frames_into_break >= 5) %>%
  mutate(motion_at_snap = as.factor(motion_at_snap)) %>%
  ggplot() +
  geom_density(aes(x = frames_into_break, fill = motion_at_snap)) +
  labs(x = "Break Entrance Quality",
       title = "Distribution of Quality of Break Entrances in Routes",
       subtitle = "Lower values are better") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1)) +
  facet_wrap(~routeRan)


#Graph distributions of break exit quality by route
after_cut %>%
  # filter(frames_out_break >= 5) %>%
  filter(!(routeRan %in% c("CROSS", "GO", "SCREEN")), out_distance > 0) %>%
  mutate(motion_at_snap = as.factor(motion_at_snap)) %>%
  ggplot() +
  geom_density(aes(x = frames_out_break, fill = motion_at_snap)) +
  labs(x = "Break Exit Quality",
       title = "Distribution of Quality of Break Exits in Routes",
       subtitle = "Lower values are better") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1)) +
  facet_wrap(~routeRan)

#Separate out entrances when players are in motion at the snap or not
no_motions_bc <- before_cut %>%
  filter(motion_at_snap == 0)
motions_bc <- before_cut %>%
  filter(motion_at_snap == 1)

#Significance tests for cut quality and kinematics at BREAK ENTRANCE and the cut itself
before_t <- before_cut %>%
  merge(players, by = c("nflId")) %>%
  filter(in_distance > 0) %>%
  group_by(routeRan) %>%
  summarize(enter_quality_alternative_hypo = ifelse(last(routeRan) %in% c("WHEEL", "HITCH", "FLAT", "CROSS"), "less", ifelse(last(routeRan) == "ANGLE", "two.sided", "greater")),
            enter_alternative_hypo_spd_x = ifelse(last(routeRan) %in% c("ANGLE", "WHEEL", "SCREEN", "FLAT"), "less", "greater"),
            enter_alternative_hypo_spd = "two.sided",
            enter_alternative_hypo_acc_x = ifelse(last(routeRan) == "WHEEL", "greater", ifelse(last(routeRan) %in% c("ANGLE", "FLAT"), "two.sided", "less")),
            enter_alternative_hypo_acc = ifelse(last(routeRan) == "CORNER", "less", ifelse(last(routeRan) %in% c("ANGLE", "HITCH"), "two.sided", "greater")),
            enter_alternative_hypo_forceimpulse = "two.sided",
            enter_quality_t_stat = t.test(frames_into_break[motion_at_snap == 0], 
                            frames_into_break[motion_at_snap == 1], 
                            alternative = enter_quality_alternative_hypo, 
                            var.equal = FALSE)$statistic,
            enter_quality_p_value = t.test(frames_into_break[motion_at_snap == 0], 
                             frames_into_break[motion_at_snap == 1], 
                             alternative = enter_quality_alternative_hypo, 
                             var.equal = FALSE)$p.value,
            times_run = n(),
            mean_in_distance = mean(in_distance),
            avg_e_spd_x = mean(enter_spd_x),
            avg_e_spd = mean(enter_spd),
            avg_c_spd_x = mean(cut_spd_x),
            avg_c_spd = mean(cut_spd),
            enter_avg_acc_x = mean(in_accel_x),
            enter_avg_acc = mean(in_accel),
            e_spd_x_t_stat = t.test(enter_spd_x[motion_at_snap == 0], 
                                  enter_spd_x[motion_at_snap == 1], 
                            alternative = enter_alternative_hypo_spd_x, 
                            var.equal = FALSE)$statistic,
            e_spd_x_p_value = t.test(enter_spd_x[motion_at_snap == 0], 
                                   enter_spd_x[motion_at_snap == 1], 
                             alternative = enter_alternative_hypo_spd_x, 
                             var.equal = FALSE)$p.value,
            e_spd_t_stat = t.test(enter_spd[motion_at_snap == 0], 
                                  enter_spd[motion_at_snap == 1], 
                            alternative = enter_alternative_hypo_spd, 
                            var.equal = FALSE)$statistic,
            e_spd_p_value = t.test(enter_spd[motion_at_snap == 0], 
                                   enter_spd[motion_at_snap == 1], 
                             alternative = enter_alternative_hypo_spd, 
                             var.equal = FALSE)$p.value,
            c_spd_x_t_stat = t.test(cut_spd_x[motion_at_snap == 0], 
                                    cut_spd_x[motion_at_snap == 1], 
                                    alternative = enter_alternative_hypo_spd_x, 
                                    var.equal = FALSE)$statistic,
            c_spd_x_p_value = t.test(cut_spd_x[motion_at_snap == 0], 
                                     cut_spd_x[motion_at_snap == 1], 
                                     alternative = enter_alternative_hypo_spd_x, 
                                     var.equal = FALSE)$p.value,
            c_spd_t_stat = t.test(cut_spd[motion_at_snap == 0], 
                                    cut_spd[motion_at_snap == 1], 
                                    alternative = enter_alternative_hypo_spd, 
                                    var.equal = FALSE)$statistic,
            c_spd_p_value = t.test(cut_spd[motion_at_snap == 0], 
                                     cut_spd[motion_at_snap == 1], 
                                     alternative = enter_alternative_hypo_spd, 
                                     var.equal = FALSE)$p.value,
            enter_acc_x_t_stat = t.test(in_accel_x[motion_at_snap == 0], 
                                    in_accel_x[motion_at_snap == 1], 
                                    alternative = enter_alternative_hypo_acc_x, 
                                    var.equal = FALSE)$statistic,
            enter_acc_x_p_value = t.test(in_accel_x[motion_at_snap == 0], 
                                     in_accel_x[motion_at_snap == 1], 
                                     alternative = enter_alternative_hypo_acc_x, 
                                     var.equal = FALSE)$p.value,
            enter_acc_t_stat = t.test(in_accel[motion_at_snap == 0], 
                                  in_accel[motion_at_snap == 1], 
                                  alternative = enter_alternative_hypo_acc, 
                                  var.equal = FALSE)$statistic,
            enter_acc_p_value = t.test(in_accel[motion_at_snap == 0], 
                                   in_accel[motion_at_snap == 1], 
                                   alternative = enter_alternative_hypo_acc, 
                                   var.equal = FALSE)$p.value,
            enter_force_x_t_stat = t.test(in_force_x[motion_at_snap == 0], 
                                        in_force_x[motion_at_snap == 1], 
                                        alternative = enter_alternative_hypo_forceimpulse, 
                                        var.equal = FALSE)$statistic,
            enter_force_x_p_value = t.test(in_force_x[motion_at_snap == 0], 
                                         in_force_x[motion_at_snap == 1], 
                                         alternative = enter_alternative_hypo_forceimpulse, 
                                         var.equal = FALSE)$p.value,
            enter_force_t_stat = t.test(in_force[motion_at_snap == 0], 
                                      in_force[motion_at_snap == 1], 
                                      alternative = enter_alternative_hypo_forceimpulse, 
                                      var.equal = FALSE)$statistic,
            enter_force_p_value = t.test(in_force[motion_at_snap == 0], 
                                       in_force[motion_at_snap == 1], 
                                       alternative = enter_alternative_hypo_forceimpulse, 
                                       var.equal = FALSE)$p.value,
            enter_impulse_x_t_stat = t.test(in_impulse_x[motion_at_snap == 0], 
                                          in_impulse_x[motion_at_snap == 1], 
                                          alternative = enter_alternative_hypo_forceimpulse, 
                                          var.equal = FALSE)$statistic,
            enter_impulse_x_p_value = t.test(in_impulse_x[motion_at_snap == 0], 
                                           in_impulse_x[motion_at_snap == 1], 
                                           alternative = enter_alternative_hypo_forceimpulse, 
                                           var.equal = FALSE)$p.value,
            enter_impulse_t_stat = t.test(in_impulse[motion_at_snap == 0], 
                                        in_impulse[motion_at_snap == 1], 
                                        alternative = enter_alternative_hypo_forceimpulse, 
                                        var.equal = FALSE)$statistic,
            enter_impulse_p_value = t.test(in_impulse[motion_at_snap == 0], 
                                         in_impulse[motion_at_snap == 1], 
                                         alternative = enter_alternative_hypo_forceimpulse, 
                                         var.equal = FALSE)$p.value
            )
View(before_t)

#Significance tests for cut quality and kinematics at BREAK EXIT and the cut itself
after_t <- after_cut %>%
  filter(out_distance > 0) %>%
  group_by(routeRan) %>%
  summarize(exit_quality_alternative_hypo = ifelse(last(routeRan) %in% c("WHEEL", "SCREEN", "HITCH", "FLAT", "CROSS"), "greater", ifelse(last(routeRan) == "ANGLE", "two.sided", "less")),
            exit_alternative_hypo_spd_x = ifelse(last(routeRan) %in% c("ANGLE", "WHEEL", "SCREEN", "FLAT", "HITCH"), "less", "greater"),
            exit_alternative_hypo_spd = "two.sided",
            exit_alternative_hypo_acc_x = ifelse(last(routeRan) == "WHEEL", "greater", ifelse(last(routeRan) %in% c("ANGLE", "FLAT"), "two.sided", "less")),
            exit_alternative_hypo_acc = ifelse(last(routeRan) %in% c("CORNER", "POST", "IN", "OUT"), "less", ifelse(last(routeRan) %in% c("ANGLE", "HITCH"), "two.sided", "greater")),
            exit_alternative_hypo_forceimpulse = "two.sided",
            exit_quality_t_stat = t.test(frames_out_break[motion_at_snap == 0], 
                            frames_out_break[motion_at_snap == 1], 
                            alternative = exit_quality_alternative_hypo, 
                            var.equal = FALSE)$statistic,
            exit_quality_p_value = t.test(frames_out_break[motion_at_snap == 0], 
                             frames_out_break[motion_at_snap == 1], 
                             alternative = exit_quality_alternative_hypo, 
                             var.equal = FALSE)$p.value,
            
            times_run = n(),
            avg_x_spd_x = mean(exit_spd_x),
            avg_x_spd = mean(exit_spd),
            avg_c_spd_x = mean(cut_spd_x),
            avg_c_spd = mean(cut_spd),
            exit_avg_acc_x = mean(out_accel_x),
            exit_avg_acc = mean(out_accel),
            x_spd_x_t_stat = t.test(exit_spd_x[motion_at_snap == 0], 
                                    exit_spd_x[motion_at_snap == 1], 
                                    alternative = exit_alternative_hypo_spd_x, 
                                    var.equal = FALSE)$statistic,
            x_spd_x_p_value = t.test(exit_spd_x[motion_at_snap == 0], 
                                     exit_spd_x[motion_at_snap == 1], 
                                     alternative = exit_alternative_hypo_spd_x, 
                                     var.equal = FALSE)$p.value,
            x_spd_t_stat = t.test(exit_spd[motion_at_snap == 0], 
                                  exit_spd[motion_at_snap == 1], 
                                  alternative = exit_alternative_hypo_spd, 
                                  var.equal = FALSE)$statistic,
            x_spd_p_value = t.test(exit_spd[motion_at_snap == 0], 
                                   exit_spd[motion_at_snap == 1], 
                                   alternative = exit_alternative_hypo_spd, 
                                   var.equal = FALSE)$p.value,
            x_spd_x_t_stat = t.test(cut_spd_x[motion_at_snap == 0], 
                                    cut_spd_x[motion_at_snap == 1], 
                                    alternative = exit_alternative_hypo_spd_x, 
                                    var.equal = FALSE)$statistic,
            x_spd_x_p_value = t.test(cut_spd_x[motion_at_snap == 0], 
                                     cut_spd_x[motion_at_snap == 1], 
                                     alternative = exit_alternative_hypo_spd_x, 
                                     var.equal = FALSE)$p.value,
            c_spd_t_stat = t.test(cut_spd[motion_at_snap == 0], 
                                  cut_spd[motion_at_snap == 1], 
                                  alternative = exit_alternative_hypo_spd, 
                                  var.equal = FALSE)$statistic,
            c_spd_p_value = t.test(cut_spd[motion_at_snap == 0], 
                                   cut_spd[motion_at_snap == 1], 
                                   alternative = exit_alternative_hypo_spd, 
                                   var.equal = FALSE)$p.value,
            exit_acc_x_t_stat = t.test(out_accel_x[motion_at_snap == 0], 
                                        out_accel_x[motion_at_snap == 1], 
                                        alternative = exit_alternative_hypo_acc_x, 
                                        var.equal = FALSE)$statistic,
            exit_acc_x_p_value = t.test(out_accel_x[motion_at_snap == 0], 
                                         out_accel_x[motion_at_snap == 1], 
                                         alternative = exit_alternative_hypo_acc_x, 
                                         var.equal = FALSE)$p.value,
            exit_acc_t_stat = t.test(out_accel[motion_at_snap == 0], 
                                      out_accel[motion_at_snap == 1], 
                                      alternative = exit_alternative_hypo_acc, 
                                      var.equal = FALSE)$statistic,
            exit_acc_p_value = t.test(out_accel[motion_at_snap == 0], 
                                       out_accel[motion_at_snap == 1], 
                                       alternative = exit_alternative_hypo_acc, 
                                       var.equal = FALSE)$p.value,
            exit_force_x_t_stat = t.test(out_force_x[motion_at_snap == 0], 
                                          out_force_x[motion_at_snap == 1], 
                                          alternative = exit_alternative_hypo_forceimpulse, 
                                          var.equal = FALSE)$statistic,
            exit_force_x_p_value = t.test(out_force_x[motion_at_snap == 0], 
                                           out_force_x[motion_at_snap == 1], 
                                           alternative = exit_alternative_hypo_forceimpulse, 
                                           var.equal = FALSE)$p.value,
            exit_force_t_stat = t.test(out_force[motion_at_snap == 0], 
                                        out_force[motion_at_snap == 1], 
                                        alternative = exit_alternative_hypo_forceimpulse, 
                                        var.equal = FALSE)$statistic,
            exit_force_p_value = t.test(out_force[motion_at_snap == 0], 
                                         out_force[motion_at_snap == 1], 
                                         alternative = exit_alternative_hypo_forceimpulse, 
                                         var.equal = FALSE)$p.value,
            exit_impulse_x_t_stat = t.test(out_impulse_x[motion_at_snap == 0], 
                                            out_impulse_x[motion_at_snap == 1], 
                                            alternative = exit_alternative_hypo_forceimpulse, 
                                            var.equal = FALSE)$statistic,
            exit_impulse_x_p_value = t.test(out_impulse_x[motion_at_snap == 0], 
                                             out_impulse_x[motion_at_snap == 1], 
                                             alternative = exit_alternative_hypo_forceimpulse, 
                                             var.equal = FALSE)$p.value,
            exit_impulse_t_stat = t.test(out_impulse[motion_at_snap == 0], 
                                          out_impulse[motion_at_snap == 1], 
                                          alternative = exit_alternative_hypo_forceimpulse, 
                                          var.equal = FALSE)$statistic,
            exit_impulse_p_value = t.test(out_impulse[motion_at_snap == 0], 
                                           out_impulse[motion_at_snap == 1], 
                                           alternative = exit_alternative_hypo_forceimpulse, 
                                           var.equal = FALSE)$p.value,
            mean_out_distance_nm = mean(out_distance[motion_at_snap == 0]),
            mean_out_distance_m = mean(out_distance[motion_at_snap == 1]))
View(after_t)

#Check for significance in all the tests
alpha <- 0.1 #This sets the alpha value
route_directions <- merge(before_t, after_t %>% select(-times_run, -avg_c_spd, -avg_c_spd_x, -c_spd_t_stat, -c_spd_p_value), by = c("routeRan")) %>%
  merge(route_diffs_t %>% select(-times_run)) %>%
  mutate(c_spd_significant = ifelse(c_spd_p_value < alpha, 1, 0),
         c_spd_x_significant = ifelse(c_spd_x_p_value < alpha, 1, 0),
         cut_significant = ifelse(cut_p_value < alpha, 1, 0),
         e_spd_significant = ifelse(e_spd_p_value < alpha, 1, 0),
         e_spd_x_significant = ifelse(e_spd_x_p_value < alpha, 1, 0),
         enter_acc_significant = ifelse(enter_acc_p_value < alpha, 1, 0),
         enter_acc_x_significant = ifelse(enter_acc_x_p_value < alpha, 1, 0),
         enter_force_significant = ifelse(enter_force_p_value < alpha, 1, 0),
         enter_force_x_significant = ifelse(enter_force_x_p_value < alpha, 1, 0),
         enter_impulse_significant = ifelse(enter_impulse_p_value < alpha, 1, 0),
         enter_impulse_x_significant = ifelse(enter_impulse_x_p_value < alpha, 1, 0),
         enter_quality_significant = ifelse(enter_quality_p_value < alpha, 1, 0),
         exit_acc_significant = ifelse(exit_acc_p_value < alpha, 1, 0),
         exit_acc_x_significant = ifelse(exit_acc_x_p_value < alpha, 1, 0),
         exit_force_significant = ifelse(exit_force_p_value < alpha, 1, 0),
         exit_force_x_significant = ifelse(exit_force_x_p_value < alpha, 1, 0),
         exit_impulse_significant = ifelse(exit_impulse_p_value < alpha, 1, 0),
         exit_impulse_x_significant = ifelse(exit_impulse_x_p_value < alpha, 1, 0),
         exit_quality_significant = ifelse(exit_quality_p_value < alpha, 1, 0),
         exit_spd_significant = ifelse(x_spd_p_value < alpha, 1, 0),
         exit_spd_x_significant = ifelse(x_spd_x_p_value < alpha, 1, 0))
# sort(names(route_directions))
# a3 <- names(route_directions)
# # a4 <- a3[!(grepl("t_stat", a3))]
# a5 <- a3[!(grepl("p_value", a3))]
# a6 <- a5[!(grepl("alternative_hypo", a5))]
# a7 <- a6[!(grepl("avg_", a6))]

# View(route_directions %>%
#   select(a5, -mean_in_distance, -mean_out_distance_nm, -mean_out_distance_m))

#These functions set color scales for the gt tables
pal1 <- function(x) {
  f1 <- scales::col_bin(
    palette = "red",
    bin = c(-Inf, -2)
  )
  f2 <- scales::col_numeric(
    palette = c("red", "white"),
    domain = c(-2, 0)
  )
  f3 <- scales::col_numeric(
    palette = c("white", "#2daa22"),
    domain = c(0, 2)
  )
  f4 <- scales::col_bin(
    palette = "#2daa22",
    bin = c(2, Inf)
  )
  
  dplyr::case_when(
    x < -2 ~ f1(x),
    x < 0 ~ f2(x),
    x < 2 ~ f3(x),
    x < Inf ~ f4(x)
  )
}

pal2 <- function(x) {
  f1 <- scales::col_bin(
    palette = "red",
    bin = c(2, Inf)
  )
  f2 <- scales::col_numeric(
    palette = c("white", "red"),
    domain = c(0, 2)
  )
  f3 <- scales::col_numeric(
    palette = c("#2daa22", "white"),
    domain = c(-2, 0)
  )
  f4 <- scales::col_bin(
    palette = "#2daa22",
    bin = c(-Inf, -2)
  )
  
  dplyr::case_when(
    x < -2 ~ f4(x),
    x < 0 ~ f3(x),
    x < 2 ~ f2(x),
    x < Inf ~ f1(x)
  )
}

pal3 <- function(x) {
  f1 <- scales::col_bin(
    palette = "#ffa53f",
    bin = c(-Inf, -2)
  )
  f2 <- scales::col_numeric(
    palette = c("#ffa53f", "white"),
    domain = c(-2, 0)
  )
  f3 <- scales::col_numeric(
    palette = c("white", "#bf77f6"),
    domain = c(0, 2)
  )
  f4 <- scales::col_bin(
    palette = "#bf77f6",
    bin = c(2, Inf)
  )
  
  dplyr::case_when(
    x < -2 ~ f1(x),
    x < 0 ~ f2(x),
    x < 2 ~ f3(x),
    x < Inf ~ f4(x)
  )
}

#Get the data setup and formatted for the GT tables
for_gt <- route_directions %>%
  filter(!(routeRan %in% c("CROSS", "GO", "SCREEN"))) %>%
  select(all_of(a7), -mean_in_distance, -mean_out_distance_nm, -mean_out_distance_m,
         -cut_direction, -times_run, -mean_cut_angle_motion, -mean_cut_angle_no_motion) %>%
  mutate(c_spd_t_stat = round(c_spd_t_stat, 2),
         c_spd_x_t_stat = round(c_spd_x_t_stat, 2),
         cut_t_stat = round(cut_t_stat, 2),
         e_spd_t_stat = round(e_spd_t_stat, 2),
         e_spd_x_t_stat = round(e_spd_x_t_stat, 2),
         enter_acc_t_stat = round(enter_acc_t_stat, 2),
         enter_acc_x_t_stat = round(enter_acc_x_t_stat, 2),
         enter_force_t_stat = round(enter_force_t_stat, 2),
         enter_force_x_t_stat = round(enter_force_x_t_stat, 2),
         enter_impulse_t_stat = round(enter_impulse_t_stat, 2),
         enter_impulse_x_t_stat = round(enter_impulse_x_t_stat, 2),
         enter_quality_t_stat = round(enter_quality_t_stat, 2),
         exit_acc_t_stat = round(exit_acc_t_stat, 2),
         exit_acc_x_t_stat = round(exit_acc_x_t_stat, 2),
         exit_force_t_stat = round(exit_force_t_stat, 2),
         exit_force_x_t_stat = round(exit_force_x_t_stat, 2),
         exit_impulse_t_stat = round(exit_impulse_t_stat, 2),
         exit_impulse_x_t_stat = round(exit_impulse_x_t_stat, 2),
         exit_quality_t_stat = round(exit_quality_t_stat, 2),
         x_spd_t_stat = round(x_spd_t_stat, 2),
         x_spd_x_t_stat = round(x_spd_x_t_stat, 2),
         c_spd_significance = ifelse(c_spd_significant == 0, "No Difference", ifelse(c_spd_t_stat < 0, "Lower", "Higher")),
         c_spd_x_significance = ifelse(c_spd_x_significant == 0, "No Difference", ifelse(c_spd_x_t_stat < 0, "Lower", "Higher")),
         cut_significance = ifelse(cut_significant == 0, "No Difference", ifelse(cut_t_stat < 0, "Lower", "Higher")),
         e_spd_significance = ifelse(e_spd_significant == 0, "No Difference", ifelse(e_spd_t_stat < 0, "Lower", "Higher")),
         e_spd_x_significance = ifelse(e_spd_x_significant == 0, "No Difference", ifelse(e_spd_x_t_stat < 0, "Lower", "Higher")),
         enter_acc_significance = ifelse(enter_acc_significant == 0, "No Difference", ifelse(enter_acc_t_stat < 0, "Lower", "Higher")),
         enter_acc_x_significance = ifelse(enter_acc_x_significant == 0, "No Difference", ifelse(enter_acc_x_t_stat < 0, "Lower", "Higher")),
         enter_force_significance = ifelse(enter_force_significant == 0, "No Difference", ifelse(enter_force_t_stat < 0, "Lower", "Higher")),
         enter_force_x_significance = ifelse(enter_force_x_significant == 0, "No Difference", ifelse(enter_force_x_t_stat < 0, "Lower", "Higher")),
         enter_impulse_significance = ifelse(enter_impulse_significant == 0, "No Difference", ifelse(enter_impulse_t_stat < 0, "Lower", "Higher")),
         enter_impulse_x_significance = ifelse(enter_impulse_x_significant == 0, "No Difference", ifelse(enter_impulse_x_t_stat < 0, "Lower", "Higher")),
         enter_quality_significance = ifelse(enter_quality_significant == 0, "No Difference", ifelse(enter_quality_t_stat > 0, "Lower", "Higher")),
         exit_acc_significance = ifelse(exit_acc_significant == 0, "No Difference", ifelse(exit_acc_t_stat < 0, "Lower", "Higher")),
         exit_acc_x_significance = ifelse(exit_acc_x_significant == 0, "No Difference", ifelse(exit_acc_x_t_stat < 0, "Lower", "Higher")),
         exit_force_significance = ifelse(exit_force_significant == 0, "No Difference", ifelse(exit_force_t_stat < 0, "Lower", "Higher")),
         exit_force_x_significance = ifelse(exit_force_x_significant == 0, "No Difference", ifelse(exit_force_x_t_stat < 0, "Lower", "Higher")),
         exit_impulse_significance = ifelse(exit_impulse_significant == 0, "No Difference", ifelse(exit_impulse_t_stat < 0, "Lower", "Higher")),
         exit_impulse_x_significance = ifelse(exit_impulse_x_significant == 0, "No Difference", ifelse(exit_impulse_x_t_stat < 0, "Lower", "Higher")),
         exit_quality_significance = ifelse(exit_quality_significant == 0, "No Difference", ifelse(exit_quality_t_stat > 0, "Lower", "Higher")),
         exit_spd_significance = ifelse(exit_spd_significant == 0, "No Difference", ifelse(x_spd_t_stat < 0, "Lower", "Higher")),
         exit_spd_x_significance = ifelse(exit_spd_x_significant == 0, "No Difference", ifelse(x_spd_x_t_stat < 0, "Lower", "Higher")))

#Plot eveyrthing - entrance, exit, and the cut itself
for_gt %>%
  gt() %>%
  cols_hide(c("c_spd_significant", "c_spd_x_significant", "cut_significant", "e_spd_significant",
              "e_spd_x_significant", "enter_acc_significant", "enter_acc_x_significant",
              "enter_force_significant", "enter_force_x_significant", "enter_impulse_significant",
              "enter_impulse_x_significant", "enter_quality_significant", "exit_acc_significant",
              "exit_acc_x_significant", "exit_force_significant", "exit_force_x_significant",
              "exit_impulse_significant", "exit_impulse_x_significant", "exit_quality_significant",
              "exit_spd_significant", "exit_spd_x_significant")) %>%
  cols_align(align = c("center"), columns = everything()) %>%
  tab_stubhead("routeRan") %>%
  tab_style(style = cell_borders(side = c("right"), color = "black"),
            locations = list(cells_body(c("routeRan")))) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = list(cells_body(c("routeRan")),
                             cells_stubhead(),
                             cells_stub())) %>%
  gt_merge_stack(c_spd_t_stat, c_spd_significance, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = c_spd_x_significance, col1 = c_spd_x_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = cut_significance, col1 = cut_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = e_spd_significance, col1 = e_spd_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = e_spd_x_significance, col1 = e_spd_x_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = enter_acc_significance, col1 = enter_acc_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = enter_acc_x_significance, col1 = enter_acc_x_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = enter_force_significance, col1 = enter_force_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = enter_force_x_significance, col1 = enter_force_x_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = enter_impulse_significance, col1 = enter_impulse_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = enter_impulse_x_significance, col1 = enter_impulse_x_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = enter_quality_significance, col1 = enter_quality_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = exit_acc_significance, col1 = exit_acc_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = exit_acc_x_significance, col1 = exit_acc_x_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = exit_force_significance, col1 = exit_force_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = exit_force_x_significance, col1 = exit_force_x_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = exit_impulse_significance, col1 = exit_impulse_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = exit_impulse_x_significance, col1 = exit_impulse_x_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = exit_quality_significance, col1 = exit_quality_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = exit_spd_significance, col1 = x_spd_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = exit_spd_x_significance, col1 = x_spd_x_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  cols_label(
    cut_t_stat = "Cut Angle",
    c_spd_t_stat = "Speed at Cut",
    c_spd_x_t_stat = "Speed in X Direction at Cut",
    e_spd_t_stat = "Speed Entering Cut",
    e_spd_x_t_stat = "Speed in X Direction Entering Cut",
    enter_acc_t_stat = "Deceleration of Cut",
    enter_acc_x_t_stat = "Deceleration of Cut in X Direction",
    enter_force_t_stat = "Average Force of Cut Entrance",
    enter_force_x_t_stat = "Average Force of Cut Entrance in X Direction",
    enter_impulse_t_stat = "Impulse of Cut Entrance",
    enter_impulse_x_t_stat = "Impulse of Cut Entrance in X Direction",
    enter_quality_t_stat = "Cut Entrance Quality",
    exit_acc_t_stat = "Acceleration out of Cut",
    exit_acc_x_t_stat = "Acceleration out of Cut in X Direction",
    exit_force_t_stat = "Average Force of Cut Exit",
    exit_force_x_t_stat = "Average Force of Cut Exit in X Direction",
    exit_impulse_t_stat = "Impulse of Cut Exit",
    exit_impulse_x_t_stat = "Impulse of Cut Exit in X Direction",
    exit_quality_t_stat = "Cut Exit Quality",
    x_spd_t_stat = "Speed After Cut",
    x_spd_x_t_stat = "Speed in X Direction After Cut",
    routeRan = "Route Type"
  ) %>%
  tab_header(title = md("**Statistical Significance of Changes in Cut Dynamics by Route Designation**"),
             subtitle = md("*Between Route Runner In Motion at the Snap and Not In Motion at the Snap<br>t-Statistic on Top, Interpretation on the Bottom<br>Green is considered better when in motion, Red is considered worse when in motion*")) %>%
  data_color(columns = c(c_spd_x_t_stat, e_spd_t_stat,
                         e_spd_x_t_stat,
                         exit_acc_t_stat, exit_acc_x_t_stat,
                         exit_force_t_stat, exit_force_x_t_stat,
                         exit_impulse_t_stat, exit_impulse_x_t_stat,
                         x_spd_t_stat,
                         x_spd_x_t_stat), fn = pal1) %>%
  data_color(columns = c(enter_acc_t_stat,
                         enter_acc_x_t_stat, enter_force_t_stat,
                         enter_force_x_t_stat, enter_impulse_t_stat,
                         enter_impulse_x_t_stat, enter_quality_t_stat,
                         exit_quality_t_stat), fn = pal2) %>%
  data_color(columns = c(cut_t_stat, c_spd_t_stat), fn = pal3)

#GT table - just at the cut
at_cut <- for_gt %>%
  select(routeRan,
         cut_t_stat, cut_significance,
         c_spd_t_stat, c_spd_significance,
         c_spd_x_t_stat, c_spd_x_significance) %>%
  gt() %>%
  tab_stubhead("routeRan") %>%
  cols_align(align = c("center"), columns = everything()) %>%
  tab_style(style = cell_borders(side = c("right"), color = "black"),
                                     locations = list(cells_body(c("routeRan")))) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = list(cells_body(c("routeRan")),
                             cells_stubhead(),
                             cells_stub())) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()) %>%
  gt_merge_stack(c_spd_t_stat, c_spd_significance, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = c_spd_x_significance, col1 = c_spd_x_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = cut_significance, col1 = cut_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  cols_label(
    cut_t_stat = "Cut Angle",
    c_spd_t_stat = "Speed at Cut",
    c_spd_x_t_stat = "Speed in X Direction at Cut",
    routeRan = "Route Type") %>%
  tab_header(title = md("**Statistical Significance of Changes in Cut Dynamics by Route Designation<br>The Cut**"),
             subtitle = md("*Between Route Runner In Motion at the Snap and Not In Motion at the Snap<br>t-Statistic on Top, Interpretation on the Bottom<br>Green is considered better when in motion, Red is considered worse when in motion, Purple and Orange are neither*")) %>%
  data_color(columns = c(c_spd_x_t_stat, cut_t_stat, c_spd_t_stat), fn = pal3)

#GT table - just break entrances
entrances <- for_gt %>%
  select(routeRan,
         enter_quality_t_stat, enter_quality_significance,
         e_spd_t_stat, e_spd_significance,
         e_spd_x_t_stat, e_spd_x_significance,
         enter_acc_t_stat, enter_acc_significance,
         enter_acc_x_t_stat, enter_acc_x_significance,
         enter_force_t_stat, enter_force_significance,
         enter_force_x_t_stat, enter_force_x_significance,
         enter_impulse_t_stat, enter_impulse_significance,
         enter_impulse_x_t_stat, enter_impulse_x_significance) %>%
  gt() %>%
  tab_stubhead("routeRan") %>%
  cols_align(align = c("center"), columns = everything()) %>%
  tab_style(style = cell_borders(side = c("right"), color = "black"),
            locations = list(cells_body(c("routeRan")))) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = list(cells_body(c("routeRan")),
                             cells_stubhead(),
                             cells_stub())) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()) %>%
  gt_merge_stack(col2 = e_spd_significance, col1 = e_spd_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = e_spd_x_significance, col1 = e_spd_x_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = enter_acc_significance, col1 = enter_acc_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = enter_acc_x_significance, col1 = enter_acc_x_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = enter_force_significance, col1 = enter_force_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = enter_force_x_significance, col1 = enter_force_x_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = enter_impulse_significance, col1 = enter_impulse_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = enter_impulse_x_significance, col1 = enter_impulse_x_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = enter_quality_significance, col1 = enter_quality_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  cols_label(e_spd_t_stat = "Speed Entering Cut",
             e_spd_x_t_stat = "Speed in X Direction Entering Cut",
             enter_acc_t_stat = "Deceleration of Cut",
             enter_acc_x_t_stat = "Deceleration of Cut in X Direction",
             enter_force_t_stat = "Average Force of Cut Entrance",
             enter_force_x_t_stat = "Average Force of Cut Entrance in X Direction",
             enter_impulse_t_stat = "Impulse of Cut Entrance",
             enter_impulse_x_t_stat = "Impulse of Cut Entrance in X Direction",
             enter_quality_t_stat = "Cut Entrance Quality",
             routeRan = "Route Type") %>%
  tab_header(title = md("**Statistical Significance of Changes in Cut Dynamics by Route Designation<br>Entering the Break**"),
             subtitle = md("*Between Route Runner In Motion at the Snap and Not In Motion at the Snap<br>t-Statistic on Top, Interpretation on the Bottom<br>Green is considered better when in motion, Red is considered worse when in motion, Purple and Orange are neither*")) %>%
  data_color(columns = c(e_spd_x_t_stat, e_spd_t_stat), fn = pal1) %>%
  data_color(columns = c(enter_acc_t_stat,
                         enter_acc_x_t_stat, enter_force_t_stat,
                         enter_force_x_t_stat, enter_impulse_t_stat,
                         enter_impulse_x_t_stat, enter_quality_t_stat), fn = pal2)

#GT table - just break exits
exits <- for_gt %>%
  select(routeRan,
         exit_quality_t_stat, exit_quality_significance,
         x_spd_t_stat, exit_spd_significance,
         x_spd_x_t_stat, exit_spd_x_significance,
         exit_acc_t_stat, exit_acc_significance,
         exit_acc_x_t_stat, exit_acc_x_significance,
         exit_force_t_stat, exit_force_significance,
         exit_force_x_t_stat, exit_force_x_significance,
         exit_impulse_t_stat, exit_impulse_significance,
         exit_impulse_x_t_stat, exit_impulse_x_significance) %>%
  gt() %>%
  tab_stubhead("routeRan") %>%
  cols_align(align = c("center"), columns = everything()) %>%
  tab_style(style = cell_borders(side = c("right"), color = "black"),
            locations = list(cells_body(c("routeRan")))) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = list(cells_body(c("routeRan")),
                             cells_stubhead(),
                             cells_stub())) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()) %>%
  gt_merge_stack(col2 = exit_acc_significance, col1 = exit_acc_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = exit_acc_x_significance, col1 = exit_acc_x_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = exit_force_significance, col1 = exit_force_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = exit_force_x_significance, col1 = exit_force_x_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = exit_impulse_significance, col1 = exit_impulse_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = exit_impulse_x_significance, col1 = exit_impulse_x_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = exit_quality_significance, col1 = exit_quality_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = exit_spd_significance, col1 = x_spd_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  gt_merge_stack(col2 = exit_spd_x_significance, col1 = x_spd_x_t_stat, palette = c("grey", "#512daa"), font_weight = c("bold", "bold"), font_size = c("10px", "16px")) %>%
  cols_label(exit_acc_t_stat = "Acceleration out of Cut",
             exit_acc_x_t_stat = "Acceleration out of Cut in X Direction",
             exit_force_t_stat = "Average Force of Cut Exit",
             exit_force_x_t_stat = "Average Force of Cut Exit in X Direction",
             exit_impulse_t_stat = "Impulse of Cut Exit",
             exit_impulse_x_t_stat = "Impulse of Cut Exit in X Direction",
             exit_quality_t_stat = "Cut Exit Quality",
             x_spd_t_stat = "Speed After Cut",
             x_spd_x_t_stat = "Speed in X Direction After Cut",
             routeRan = "Route Type") %>%
  tab_header(title = md("**Statistical Significance of Changes in Cut Dynamics by Route Designation<br>Exiting the Break**"),
             subtitle = md("*Between Route Runner In Motion at the Snap and Not In Motion at the Snap<br>t-Statistic on Top, Interpretation on the Bottom<br>Green is considered better when in motion, Red is considered worse when in motion, Purple and Orange are neither*")) %>%
  data_color(columns = c(exit_acc_t_stat, exit_acc_x_t_stat,
                         exit_force_t_stat, exit_force_x_t_stat,
                         exit_impulse_t_stat, exit_impulse_x_t_stat,
                         x_spd_t_stat,
                         x_spd_x_t_stat), fn = pal1) %>%
  data_color(columns = c(exit_quality_t_stat), fn = pal2)

entrances
at_cut
exits

#APPLY EXPECTED PASS OUTCOME MODEL

#Pass in setup data - this would be for every frame
all_throws <- data.table::fread("C:/Users/Joshua/Desktop/BDB 2024-25/nfl-big-data-bowl-2025/Data for modeling x pass outcome.csv")

#Pass in the expected pass outcome model
pass_out_model <- readRDS("C:/Users/Joshua/Desktop/BDB 2024-25/nfl-big-data-bowl-2025/Pass Outcomes Model 1.rds")

#Pass in the cut times
cut_times <- read_csv("C:/Users/Joshua/Desktop/BDB 2024-25/nfl-big-data-bowl-2025/Cut times w exits.csv")

#Get the data at just the break exits, select the proper columns and set it up for predicting pass outcomes using our model
all_cut_exits <- all_throws %>%
  merge(cut_times, by.x = c("gameId", "playId", "frameId", "tgt_id"), by.y = c("gameId", "playId", "exit_time", "nflId")) %>%
  mutate(time_since_cut = frameId - cut_time) %>%
  select(pass_outcome, gameId, playId, frameId, nflId, tgt_id,
         x_throw_velo, time_since_snap, time_since_cut,
         qb_hardest, db_triangle_area, db_2line, db_13, db_23, big_angle, big_angle_db, hang_time, throw_length, dropback_type, route_throw,
         qb_tgt_x_rel_spd, qb_tgt_y_rel_spd, qb_x_target, qb_y_target,
         qb_to_sl, qb_to_los,
         tgt_to_cp, tgt_o_dir, tgt_dir_ball, target_a, tgt_a_x, tgt_a_y, target_s, target_spd_x, target_spd_y, target_height,
         pass_x, pass_y, pass_angle_rad,
         d1_final_from_cp, d1_final_x_cp, d1_final_y_cp, d1_distance_covered_x, d1_distance_covered_y, d1_to_catch, d1_distance_covered, d1_dir_tgt,
         db1_spd_x, db1_spd_y, db1_a_x, db1_a_y, d1_final_y_sl,
         d2_final_from_cp, d2_final_x_cp, d2_final_y_cp, d2_distance_covered_x, d2_distance_covered_y, d2_to_catch, d2_distance_covered, d2_dir_tgt,
         db2_spd_x, db2_spd_y, db2_a_x, db2_a_y, d2_final_y_sl,
         d3_final_from_cp, d3_final_x_cp, d3_final_y_sl, d3_distance_covered_x, d3_distance_covered_y, d3_to_catch, d3_distance_covered, d3_dir_tgt,
         db3_spd_x, db3_spd_y, db3_a_x, db3_a_y, d3_final_y_sl,
         distance_to_db1, distance_to_db2,
         cp_to_sl, cp_to_1d, cp_to_gl, d1_i_from_d2, d1_f_from_d2, close_2_condense) %>%
  select(-distance_to_db1, -distance_to_db2, -dropback_type)

#Predict using the model and calculate open score
exit_preds <- as.data.frame(
  matrix(predict(pass_out_model, as.matrix(all_cut_exits %>% select(-gameId, -playId, -frameId, -nflId, -tgt_id, -pass_outcome))), ncol = 3, byrow = T)) %>%
  bind_cols(all_cut_exits) %>%
  rename(x_comp = V1,
         x_inc = V2,
         x_int = V3) %>%
  select(gameId, playId, frameId, tgt_id, x_comp, x_int) %>%
  mutate(open_score = x_comp - (4 * x_int))

#Merge in a dataframe that includes whether the player was in motion at the snap
exit_preds_routes <- merge(exit_preds,
                           before_cut %>% select(gameId, playId, nflId, routeRan, motion_at_snap),
                           by.x = c("gameId", "playId", "tgt_id"),
                           by.y = c("gameId", "playId", "nflId"))

#Significance tests for open score by route
openness_numbers <- exit_preds_routes %>%
  group_by(routeRan) %>%
  summarize(openness_t_stat = t.test(open_score[motion_at_snap == 0], 
                                     open_score[motion_at_snap == 1], 
                                         alternative = "less", 
                                         var.equal = FALSE)$statistic,
            openness_p_value = t.test(open_score[motion_at_snap == 0], 
                                      open_score[motion_at_snap == 1], 
                                          alternative = "less", 
                                          var.equal = FALSE)$p.value,
            openness_motion = mean(open_score[motion_at_snap == 1]),
            openness_no_motion = mean(open_score[motion_at_snap == 0]),
            avg_openness = mean(open_score),
            motions = sum(motion_at_snap),
            routes = n(),
            motion_pct = mean(motion_at_snap)) %>%
  filter(!(routeRan %in% c("GO", "CROSS", "SCREEN")))

# openness_numbers

#Graph distributions of open score by route
exit_preds_routes %>%
  filter(!(routeRan %in% c("GO", "CROSS", "SCREEN"))) %>%
  mutate(motion_at_snap = as.factor(motion_at_snap)) %>%
  ggplot() +
  geom_density(aes(x = open_score, fill = motion_at_snap)) +
  labs(x = "Open Score = Completion Probability Given Target - 4 * INT Probability Given Target",
       title = 'Distribution of our "Open Score" Measure of Openness at Break Exits in Routes') +
  theme(panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1)) +
  facet_wrap(~routeRan)


#Put all the metrics into the same place
merged_routes <- Reduce(function(x, y) merge(x, y, by = c("gameId", "playId", "nflId"), all = TRUE), list(before_cut %>% select(gameId, playId, nflId, frames_into_break, enter_spd_x, enter_spd, in_accel_x, in_accel, in_force_x, in_force, in_impulse_x, in_impulse),
                                                                                                               after_cut %>% select(gameId, playId, nflId, frames_out_break, exit_spd_x, exit_spd, out_accel_x, out_accel, out_force_x, out_force, out_impulse_x, out_impulse, cut_spd_x, cut_spd, height, weight),
                                                                                                               route_diffs %>% select(gameId, playId, nflId, cut_angle),
                                                                                                               exit_preds_routes %>% rename(nflId = tgt_id) %>% select(-frameId)))
plot_list = list()


#Get correlation tables for each route
cor_plot <- (cor(merged_routes %>% filter(routeRan == "GO") %>% select(-gameId, -playId, -nflId, -height, -routeRan), use = "pairwise.complete.obs"))
cor_data <- melt(cor_plot)
colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")
cor_data <- cor_data %>%
  mutate(routeRan = "GO")
for(route in unique(merged_routes$routeRan)){
  if(!(is.na(route)) && !(route %in% c("GO", "SCREEN", "CROSS"))){
    cor_plot <- (cor(merged_routes %>% filter(routeRan == route) %>% select(-gameId, -playId, -nflId, -height, -routeRan), use = "pairwise.complete.obs"))
    
    cor_data_route <- melt(cor_plot)
    
    colnames(cor_data_route) <- c("Variable1", "Variable2", "Correlation")
    cor_data_route <- cor_data_route %>%
      mutate(routeRan = route)
    cor_data <- rbind(cor_data, cor_data_route)
  }
}

# Tile plot of correlations between all the kinematics and the openness numbers
tile_plot <- cor_data %>%
  filter(Variable2 %in% c("motion_at_snap", "open_score", "x_comp", "x_int"),
         !(Variable1 %in% c("motion_at_snap", "open_score", "x_comp", "x_int")),
         routeRan != "GO") %>%
  mutate(Variable2 = case_when(Variable2 == "motion_at_snap" ~ "WR In Motion at Snap",
                               Variable2 == "open_score" ~ "Open Score",
                               Variable2 == "x_int" ~ "INT Probability",
                               Variable2 == "x_comp" ~ "Completion Probability",
                               Variable2 == "frames_into_break" ~ "Cut Entrance Quality",
                               Variable2 == "enter_spd_x" ~ "Speed in X Direction Entering Cut",
                               Variable2 == "enter_spd" ~ "Speed Entering Cut",
                               Variable2 == "in_accel_x" ~ "Deceleration in X Direction Entering Cut",
                               Variable2 == "in_accel" ~ "Deceleration Entering Cut",
                               Variable2 == "in_force_x" ~ "Force in X Direction Entering Cut",
                               Variable2 == "in_force" ~ "Force Entering Cut",
                               Variable2 == "in_impulse_x" ~ "Impulse in X Direction Entering Cut",
                               Variable2 == "in_impulse" ~ "Impulse Entering Cut",
                               Variable2 == "frames_out_break" ~ "Cut Exit Quality",
                               Variable2 == "exit_spd_x" ~ "Speed in X Direction Exiting Cut",
                               Variable2 == "exit_spd" ~ "Speed Exiting Cut",
                               Variable2 == "out_accel_x" ~ "Acceleration in X Direction Exiting Cut",
                               Variable2 == "out_accel" ~ "Acceleration Exiting Cut",
                               Variable2 == "out_force_x" ~ "Force in X Direction Exiting Cut",
                               Variable2 == "out_force" ~ "Force Exiting Cut",
                               Variable2 == "out_impulse_x" ~ "Impulse in X Direction Exiting Cut",
                               Variable2 == "out_impulse" ~ "Impulse Exiting Cut",
                               Variable2 == "cut_spd_x" ~ "Speed in X Direction at Cut",
                               Variable2 == "cut_spd" ~ "Speed at Cut",
                               Variable2 == "weight" ~ "Receiver Weight",
                               Variable2 == "cut_angle" ~ "Cut Angle",
                               T ~ "Other"),
         Variable1 = case_when(Variable1 == "motion_at_snap" ~ "WR In Motion at Snap",
                               Variable1 == "open_score" ~ "Open Score",
                               Variable1 == "x_int" ~ "INT Probability",
                               Variable1 == "x_comp" ~ "Completion Probability",
                               Variable1 == "frames_into_break" ~ "Cut Entrance Quality",
                               Variable1 == "enter_spd_x" ~ "Speed in X Direction Entering Cut",
                               Variable1 == "enter_spd" ~ "Speed Entering Cut",
                               Variable1 == "in_accel_x" ~ "Deceleration in X Direction Entering Cut",
                               Variable1 == "in_accel" ~ "Deceleration Entering Cut",
                               Variable1 == "in_force_x" ~ "Force in X Direction Entering Cut",
                               Variable1 == "in_force" ~ "Force Entering Cut",
                               Variable1 == "in_impulse_x" ~ "Impulse in X Direction Entering Cut",
                               Variable1 == "in_impulse" ~ "Impulse Entering Cut",
                               Variable1 == "frames_out_break" ~ "Cut Exit Quality",
                               Variable1 == "exit_spd_x" ~ "Speed in X Direction Exiting Cut",
                               Variable1 == "exit_spd" ~ "Speed Exiting Cut",
                               Variable1 == "out_accel_x" ~ "Deceleration in X Direction Exiting Cut",
                               Variable1 == "out_accel" ~ "Deceleration Exiting Cut",
                               Variable1 == "out_force_x" ~ "Force in X Direction Exiting Cut",
                               Variable1 == "out_force" ~ "Force Exiting Cut",
                               Variable1 == "out_impulse_x" ~ "Impulse in X Direction Exiting Cut",
                               Variable1 == "out_impulse" ~ "Impulse Exiting Cut",
                               Variable1 == "cut_spd_x" ~ "Speed in X Direction at Cut",
                               Variable1 == "cut_spd" ~ "Speed at Cut",
                               Variable1 == "weight" ~ "Receiver Weight",
                               Variable1 == "cut_angle" ~ "Cut Angle",
                               T ~ "Other")) %>%
  mutate(corr_size = abs(Correlation)) %>%
  ggplot(aes(x = Variable1, y = Variable2, fill = corr_size)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "gray", high = "orange", midpoint = 0.25) +
  labs(
    title = paste("Correlation Matrix By Route"),
    x = "Variable 1",
    y = "Variable 2",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap(~routeRan, ncol = 3)
print(tile_plot)

#Separate out one play for visualizing where the cut occurs
cut_viz_setup <- cut_quality_setup %>% select(gameId, playId, nflId, frameId, x, y, cut_time) %>%
  filter(frameId == cut_time) %>%
  select(-frameId) %>%
  rename(cut_x = x, cut_y = y) %>%
  merge(cut_quality_setup %>% select(gameId, playId, nflId, frameId, x, y), by = c("gameId", "playId", "nflId"))

#Visualize cut identification on 1 play
cut_viz <- cut_viz_setup %>%
  filter(gameId == 2022092502, playId == 64, frameId == 190) %>%
  arrange(gameId, playId, nflId, frameId) %>%
  ggplot() +
  xlim(0, 160/3) +
  ylim(0, 120) +
  labs(title = "Visualizing Cut Identification") +
  geom_hline(yintercept = 10, color = "white", linewidth = 3) +
  geom_hline(yintercept = 110, color = "white", linewidth = 3) +
  geom_hline(yintercept = seq(15, 105, by = 10), color = "white", linewidth = .6) +
  geom_hline(yintercept = seq(20, 100, by = 10), color = "white", linewidth = 1) +
  geom_vline(xintercept = 0, color = "white", linewidth = 2.5) +
  geom_vline(xintercept = 160/3, color = "white", linewidth = 2.5) +
  geom_point(aes(x = y, y = x), color = "orange", size = 6) +
  geom_text(aes(x = y, y = x), label = "WR", size = 3) +
  geom_point(aes(x = cut_y, y = cut_x), color = "red", size = 4) +
  geom_text(aes(x = cut_y, y = cut_x), label = "CUT") +
  geom_point(x = 0, y = 10, color = "#ff4d02", size = 4, shape = 15) +
  geom_point(x = 0, y = 110, color = "#ff4d02", size = 4, shape = 15) +
  geom_point(x = 53, y = 10, color = "#ff4d02", size = 4, shape = 15) +
  geom_point(x = 53, y = 110, color = "#ff4d02", size = 4, shape = 15) +
  # transition_time(frameId) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.background = element_rect(fill = "#7eaf34"),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(face = "bold", size = 15)
  )
cut_viz
