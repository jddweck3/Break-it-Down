library(tidyverse)
library(matrixStats)
library(gganimate)
library(av)
library(xgboost)

model_data_base <- all_throws %>%
  filter(tgt_id == target_id, frameId == throw_frame)

cut_times <- read_csv("C:/Users/Joshua/Desktop/BDB 2024-25/nfl-big-data-bowl-2025/Cut Times.csv")

model_data <- model_data_base %>%
  merge(cut_times, by.x = c("gameId", "playId", "tgt_id"), by.y = c("gameId", "playId", "nflId")) %>%
  mutate(time_since_cut = frameId - cut_time) %>%
  select(pass_outcome, gameId, playId, frameId, nflId,
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

gameids <- unique(model_data$gameId)
train_games <- 1:85



val_games <- 86:111
test_games <- 112:length(gameids)

train_data <- model_data %>%
  filter(gameId %in% gameids[train_games])

val_data <- model_data %>%
  filter(gameId %in% gameids[val_games])

test_data <- model_data %>%
  filter(gameId %in% gameids[test_games])

full_train <- xgboost::xgb.DMatrix(as.matrix(train_data %>% select(-gameId, -playId, -frameId, -nflId, -pass_outcome)),
                                   label = train_data$pass_outcome
)

errors <- list()
for(i in c(75)){
  for(j in c(.13)){
    for(k in c(3)){
      for(l in c(3)){
        for(m in c(1)){
          for(n in c(1)){
            for(o in c(0.05)){
              nrounds <- i
              params <-
                list(booster = "gbtree",
                     objective = "multi:softprob",
                     eval_metric = "mlogloss",
                     eta = j,
                     gamma = o,
                     subsample = n,
                     max_depth = k,
                     min_child_weight = l,
                     colsample_bytree = m,
                     num_class = 3)
              pass_out_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)
              preds <- as.data.frame(
                matrix(predict(pass_out_model, as.matrix(test_data %>% select(-gameId, -playId, -frameId, -nflId, -pass_outcome))), ncol = 3, byrow = T)) %>%
                bind_cols(test_data) %>%
                rename(x_comp = V1,
                       x_inc = V2,
                       x_int = V3) %>%
                pivot_longer(c(x_comp, x_inc, x_int), names_to = "type", values_to = "pred") %>%
                select(pass_outcome, type, pred) %>%
                mutate(type_pred = ifelse(type == "x_comp", 0, ifelse(type == "x_inc", 1, 2)),
                       right_type = ifelse(type_pred == pass_outcome, 1, 0),
                       bin_pred_prob = round(pred/.05) * .05)

              check <- preds %>%
                group_by(type, bin_pred_prob) %>%
                summarize(outcomes = mean(right_type),
                          n_throws = n(),
                          n_correct = sum(right_type))

              cp_importance <- xgb.importance(model = pass_out_model)
              xgb.plot.importance(importance_matrix = cp_importance, top_n = 20)

              check %>%
                ungroup() %>%
                ggplot(aes(x = bin_pred_prob, y = outcomes)) +
                geom_abline(slope = 1, intercept = 0, color = "red") +
                geom_point(aes(size = n_throws)) +
                geom_smooth(method = "loess") +
                scale_x_continuous(limits = c(0, 1)) +
                scale_y_continuous(limits = c(0, 1)) +
                facet_wrap(~ifelse(type == "x_comp", "COMPLETION", ifelse(type == "x_inc", "INCOMPLETION", "INTERCEPTION")))

              cv_cal_error <- check %>%
                ungroup() %>%
                mutate(diff = abs(outcomes - bin_pred_prob)) %>%
                group_by(type) %>%
                summarize(weight_error = weighted.mean(diff, n_throws, na.rm = T),
                          accs = sum(n_correct, na.rm = T))
              glue::glue(
                "
                --CALIBRATION ERROR--
                {round(with(cv_cal_error, weighted.mean(weight_error, accs)), 4)}
                "
              )
              print(round(with(cv_cal_error, weighted.mean(weight_error, accs)), 4))
              errors[[length(errors) + 1]] <- c(i, j, k, l, m, n, o, round(with(cv_cal_error, weighted.mean(weight_error, accs)), 4))
            }
          }
        }
      }
    }
  }
}
error_df <- as.data.frame(do.call(rbind, errors))
colnames(error_df) <- c("nrounds", "eta", "max_depth", "min_child_weight", "colsample_bytree", "subsample", "gamma", "error")
#View(error_df %>% arrange(error))

write_rds(pass_out_model, "C:/Users/Joshua/Desktop/BDB 2024-25/nfl-big-data-bowl-2025/Pass Outcomes Model 1.rds")

cut_times <- cut_times %>%
  mutate(compare_time = cut_time + 2)