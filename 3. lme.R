
#---------------------#
#### LOAD PACKAGES ####
#---------------------#

library(tidyverse)
library(psme)

#------------------------#
#### UTILITY FUNCTION ####
#------------------------#

xTraj <- function(model, new.x) {
  pop <- model$pcoef[[1]] + psme:::EvalSmooth(model$smooth[[1]], new.x)
  sub <- pop + psme:::EvalSmooth(model$smooth[[2]], new.x)
  list(pop = pop, sub = sub)
}

#-------------------------------#
#### FIT+PREDICT LME IN PSME ####
#-------------------------------#

load("alsp_dep_dat.RData")

original_datasets <- list(M = dat_M, F = dat_F)

fit_psme <- function(data) {
  psme(
    dep ~ s(agey, bs = 'ps', k = 7, m = c(2, 2)) +
      s(agey, id, bs = 'fs', xt = 'ps', k = 6, m = c(2, 1)),
    data = data
  )
}

system.time({
  psme_models <- map(original_datasets, fit_psme)
})

# user  system elapsed 
# 18.55    1.61   20.71 fe k = 7 

prediction_grids <- map(original_datasets, ~seq(10, 27, length.out = 200))

system.time(psme_predictions <- map2(
  psme_models, prediction_grids, function(model, age_seq) {

    traj_data <- as.data.frame(xTraj(model, age_seq)$sub)
    
    traj_data %>% 
      mutate(age = age_seq) %>% 
      pivot_longer(
        cols = starts_with("V"), 
        names_to = "id_char", 
        values_to = "pred_dep"
      ) %>%
      mutate(id = as.integer(str_extract(id_char, "\\d+"))) %>%
      select(id, age, pred_dep) %>%
      arrange(id, age)
  }
)
)

#----------------#
#### SAVE OUT ####
#----------------#

rm(original_datasets, prediction_grids, fit_psme, xTraj)

save.image("dep_psme_mods.RData")

rm(list = ls())
