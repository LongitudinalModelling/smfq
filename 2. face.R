
#---------------------#
#### LOAD PACKAGES ####
#---------------------#

library(tidyverse)
library(face)

#--------------------------------#
#### FIT+PREDICT FPCA IN FACE ####
#--------------------------------#

load("alsp_dep_dat.RData")

original_datasets <- list(M = dat_M, F = dat_F)

prepare_face_data <- function(data) {
  dat_face <- data.frame(y = data$dep, argvals = data$agey, subj = data$id)
  
  new_argvals <- seq(10, 27, length.out = 200)
  
  pred_dat <- bind_rows(
    dat_face, expand.grid(
      subj = unique(dat_face$subj), 
      argvals = new_argvals)) %>% 
    arrange(subj, argvals) %>% 
    distinct(subj, argvals, .keep_all = T)
  
  list(dat_face = dat_face, pred_dat = pred_dat)
}

prepared_list <- map(original_datasets, prepare_face_data)

face_datasets <- map(prepared_list, "dat_face")
pred_datasets <- map(prepared_list, "pred_dat")

fit_face <- function(data, prediction_data) {
  face.sparse(
    data, 
    knots = 6,
    calculate.scores = T,
    argvals.new = seq(10, 27, length.out = 200), 
    newdata = prediction_data
  )
}

system.time({face_models <- map2(
  face_datasets, pred_datasets, ~fit_face(.x, .y))
})

face_predictions <- map2(
  face_models, pred_datasets, function(model, pred_df) {
    pred_df %>% mutate(
      pred_dep = model$y.pred, id = as.integer(subj)) %>%
      rename(age = argvals) %>% select(id, age, pred_dep)
  })

#----------------#
#### SAVE OUT ####
#----------------#

rm(original_datasets, prepared_list, face_datasets, pred_datasets, 
   fit_face, prepare_face_data)

save.image("dep_face_mods.RData")

rm(list = ls())
