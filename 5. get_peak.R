
#---------------------#
#### LOAD PACKAGES ####
#---------------------#

library(psme)
library(face)
library(sitar)
library(tidyverse)

#-----------------------#
#### GET PEAK/PV DEP ####
#-----------------------#

get_pdep <- function(df, age_limit) {
  tmp <- df %>% filter(age <= age_limit) %>% split(.$id)
  map_df(tmp, ~ getPeak(x = .x$age, y = .x$pred_dep))
}

get_pvdep <- function(df, age_limit) {
  tmp <- df %>% filter(age <= age_limit) %>% split(.$id)
  map_df(tmp, ~ getPeak(x = .x$age, y = .x$pred_dep, Dy = T))
}

# PSME

load("dep_psme_mods.RData")

est_M <- get_pdep(psme_predictions$M, 20) %>%
  mutate(id = unique(psme_predictions$M$id), sex = "Males") %>%
  rename(PeakDepAge_psme = x, PeakDep_psme = y)

est_M <- get_pvdep(psme_predictions$M, 20) %>%
  mutate(id = unique(psme_predictions$M$id)) %>%
  rename(APVDep_psme = x, PVDep_psme = y) %>% 
  full_join(est_M)

est_F <- get_pdep(psme_predictions$F, 20) %>%
  mutate(id = unique(psme_predictions$F$id), sex = "Females") %>%
  rename(PeakDepAge_psme = x, PeakDep_psme = y)

est_F <- get_pvdep(psme_predictions$F, 20) %>%
  mutate(id = unique(psme_predictions$F$id)) %>%
  rename(APVDep_psme = x, PVDep_psme = y) %>% 
  full_join(est_F)

# FACE

load("dep_face_mods.RData")

face_predictions$M <- face_predictions$M %>% filter(age %in% unique(psme_predictions$M$age))
face_predictions$F <- face_predictions$F %>% filter(age %in% unique(psme_predictions$F$age))

est_M <- get_pdep(face_predictions$M, 20) %>%
  mutate(id = unique(face_predictions$M$id)) %>%
  rename(PeakDepAge_face = x, PeakDep_face = y) %>% 
  full_join(est_M)

est_M <- get_pvdep(face_predictions$M, 20) %>%
  mutate(id = unique(face_predictions$M$id)) %>%
  rename(APVDep_face = x, PVDep_face = y) %>% 
  full_join(est_M)

est_F <- get_pdep(face_predictions$F, 20) %>%
  mutate(id = unique(face_predictions$F$id)) %>%
  rename(PeakDepAge_face = x, PeakDep_face = y) %>% 
  full_join(est_F)

est_F <- get_pvdep(face_predictions$F, 20) %>%
  mutate(id = unique(face_predictions$F$id)) %>%
  rename(APVDep_face = x, PVDep_face = y) %>% 
  full_join(est_F)

rm(get_pdep, get_pvdep, psme_predictions, face_predictions, psme_models, face_models)

# COMBINE & MERGE

est_M <- dat_M %>% mutate(id = as.integer(id)) %>% select(
  -dep, -age, -visit) %>% distinct(id, .keep_all = T) %>% 
  full_join(est_M) %>% select(-id)

est_F <- dat_F %>% mutate(id = as.integer(id)) %>% select(
  -dep, -age, -visit) %>% distinct(id, .keep_all = T) %>% 
  full_join(est_F) %>% select(-id)

est_MF <- est_M %>% bind_rows(est_F) %>% mutate(id = row_number())

rm(est_M, est_F, dat_M, dat_F)

save.image("peak_dep_dat.RData")

colSums(!is.na(est_MF))
summary(est_MF)

rm(list = ls())
