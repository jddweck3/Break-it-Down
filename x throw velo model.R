library(tidyverse)
library(xgboost)

#Feature selection
x_velo_data <- qb_target_def %>%
  select(qb_x_target, qb_y_target, qb_to_target, qb_target_angle, target_adj_o, qb_tgt_x_rel_spd, qb_tgt_y_rel_spd,
         qb_hardest, s,# target_a,
         target_spd_x, target_spd_y,
         target_to_sl, qb_to_sl,
         tgt_to_1d, tgt_to_gl, qb_to_los,
         db1_x_target, distance_to_db1, db2_x_target, distance_to_db2,
         db1_spd_x, db1_spd_y, db2_spd_x, db2_spd_y, db3_spd_x, db3_spd_y,
         db_2line, db_triangle_area, big_angle, big_angle_db,
         down, dropback_type, target_height, height_inches, route_throw,
         max_ball_speed,
         gameId, playId, nflId, throw_frame, arrive_frame, target_id) %>%
  rename(qb_speed = s)

#Split games into train and test sets - done by games to prevent leakage as much as possible
gameids <- unique(x_velo_data$gameId)
train_games <- 1:85
val_games <- 86:111
test_games <- 112:length(gameids)

train_data <- x_velo_data %>%
  filter(gameId %in% gameids[train_games])

val_data <- x_velo_data %>%
  filter(gameId %in% gameids[val_games])

test_data <- x_velo_data %>%
  filter(gameId %in% gameids[test_games])

full_train <- xgboost::xgb.DMatrix(as.matrix(train_data %>% select(-gameId, -playId, -nflId, -max_ball_speed, -throw_frame, -arrive_frame, -target_id)),
                                   label = train_data$max_ball_speed
)

#Create and test the model
errors <- c()
for(b in c(125)){
  for(a in c(.13)){
    for(e in c(14)){
      for(d in c(11)){
        nrounds <- b
        params <-
          list(
            booster = "gbtree",
            objective = "reg:absoluteerror",
            eval_metric = "rmse",
            eta = a,
            gamma = .05,
            subsample = 1,
            max_depth = d,
            min_child_weight = e
          )
  
  
        x_velo_model_1 <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)
  
  
        preds <- as.data.frame(
          matrix(predict(x_velo_model_1, as.matrix(test_data %>% select(-gameId, -playId, -nflId, -max_ball_speed, -throw_frame, -arrive_frame, -target_id))))) %>%
          dplyr::rename(x_pass_velo = V1)
        cv_results_1 <- bind_cols(test_data, preds) %>%
          mutate(error = abs(x_pass_velo - max_ball_speed))
  
        velo_importance <- xgb.importance(model = x_velo_model_1)
        xgb.plot.importance(importance_matrix = velo_importance)
        print(paste("MAE:", mean(cv_results_1$error), "RMSE:", sqrt(mean(cv_results_1$error ** 2))))
        errors <- c(errors, sqrt(mean(cv_results_1$error ** 2)))
      }
    }
  }
}
errors

#Visualize model accuracy
as.data.frame(matrix(predict(x_velo_model_1, as.matrix(x_velo_data %>% select(-gameId, -playId, -nflId, -max_ball_speed, -throw_frame, -arrive_frame, -target_id))))) %>%
  dplyr::rename(x_pass_velo = V1) %>%
  cbind(x_velo_data) %>%
  ggplot(aes(x = x_pass_velo * 2.046, y = max_ball_speed * 2.046)) +
  geom_point(color = "darkblue") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Expected Pass Velocity (mph)",
       y = "Pass Velocity (mph)") +
  xlim(0, 70) +
  ylim(0, 70)
#save model
saveRDS(x_velo_model_1, "C:/Users/Joshua/Desktop/BDB 2024-25/nfl-big-data-bowl-2025/Expected Throw Velo 1.rds")
#Use model to predict throw velocity for any throw to any player at any time and pass it out for use in expected pass outcome model
write_csv(as.data.frame(matrix(predict(x_velo_model_1, as.matrix(x_velo_data %>% select(-gameId, -playId, -nflId, -max_ball_speed, -throw_frame, -arrive_frame, -target_id))))) %>%
            dplyr::rename(x_pass_velo = V1) %>%
            cbind(x_velo_data), "C:/Users/Joshua/Desktop/BDB 2024-25/nfl-big-data-bowl-2025/Expected Throw Velo Model 1 Results.csv")