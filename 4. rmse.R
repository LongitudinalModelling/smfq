
#---------------------#
#### LOAD PACKAGES ####
#---------------------#

library(tidyverse)
library(psme)
library(face)

#------------------#
#### RMSE: FPCA ####
#------------------#

load("dep_face_mods.RData")

# MALES

face_obs_pred_M <- face_models$M$newdata
face_obs_pred_M$pred_dep <- face_models$M$y.pred
face_obs_pred_M <- face_obs_pred_M %>% rename(id = subj, agey = argvals) %>% 
  select(id, agey, pred_dep) %>% right_join(dat_M) %>% 
  select(id, agey, dep, pred_dep)

face_obs_pred_M %>% group_by(id) %>% summarise(
  rmse_id = sqrt(mean((dep - pred_dep)^2))) %>%
  pull(rmse_id) %>% mean() # 2.2

# FEMALES

face_obs_pred_F <- face_models$F$newdata
face_obs_pred_F$pred_dep <- face_models$F$y.pred
face_obs_pred_F <- face_obs_pred_F %>% rename(id = subj, agey = argvals) %>% 
  select(id, agey, pred_dep) %>% right_join(dat_F) %>% 
  select(id, agey, dep, pred_dep)

face_obs_pred_F %>% group_by(id) %>% summarise(
  rmse_id = sqrt(mean((dep - pred_dep)^2))) %>%
  pull(rmse_id) %>% mean() # 2.4

#-----------------#
#### RMSE: LME ####
#-----------------#

load("dep_psme_mods.RData")

xTraj <- function(model, new.x) {
  pop <- model$pcoef[[1]] + psme:::EvalSmooth(model$smooth[[1]], new.x)
  sub <- pop + psme:::EvalSmooth(model$smooth[[2]], new.x)
  list(pop = pop, sub = sub)
}

make_preds_unique_age <- function(models, data_obs) {
  unique_ages <- unique(data_obs$agey)
  
  tmp <- as.data.frame(xTraj(models[[1]], unique_ages)$sub) %>% 
    mutate(agey = unique_ages) %>% pivot_longer(
      cols = starts_with("V"), names_to = "id_num", 
      values_to = "pred_dep")
  
  tmp$id <- as.integer(sub("^\\D+", "", tmp$id_num))
  
  obs_data <- data_obs %>% select(id, agey, dep)
  
  final_df <- obs_data %>% mutate(id = as.integer(id)) %>% 
    left_join(tmp, by = c("id", "agey")) %>% select(
      id, agey, dep, pred_dep)
  
  return(final_df)
}

# MALES

psme_obs_pred_M <- make_preds_unique_age(list(psme_models$M), dat_M)

psme_obs_pred_M %>% group_by(id) %>% summarise(
  rmse_id = sqrt(mean((dep - pred_dep)^2))) %>%
  pull(rmse_id) %>% mean() # 2.3

# FEMALES

psme_obs_pred_F <- make_preds_unique_age(list(psme_models$F), dat_F)

psme_obs_pred_F %>% group_by(id) %>% summarise(
  rmse_id = sqrt(mean((dep - pred_dep)^2))) %>%
  pull(rmse_id) %>% mean() # 3.1

rm(list = ls())
