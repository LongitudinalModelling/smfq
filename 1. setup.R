
#---------------------#
#### LOAD PACKAGES ####
#---------------------#

library(tidyverse)

#---------------------#
#### PREP DATASETS ####
#---------------------#

dat_M <- read.table(
  "dat/alsp_smfq_long.csv", header = T, sep = ",", na.strings = c("", "NA")) %>% 
  filter(kz021 == 1 & !is.na(age) & !is.na(dep)) %>% mutate(
    kz021 = as.factor(kz021), visit = as.factor(visit), 
    bullied_010 = as.factor(bullied_010), m_dep = as.factor(m_dep),
    psych7y = as.factor(psych7y), abuse_010 = as.factor(abuse_010),
    dv_010 = as.factor(dv_010), m_ed = as.factor(m_ed),
    agey = age / 12) %>% group_by(id) %>% filter(n() >= 2) %>% 
  mutate(id = cur_group_id()) %>% ungroup()

dat_M %>% group_by(id) %>% count() %>% ungroup() %>% 
  summarise(n_distinct(id), median(n), min(n), max(n), IQR(n))

dat_M$id <- as.factor(dat_M$id)

dat_F <-read.table(
  "dat/alsp_smfq_long.csv", header = T, sep = ",", na.strings = c("", "NA")) %>% 
  filter(kz021 == 2 & !is.na(age) & !is.na(dep)) %>% mutate(
    kz021 = as.factor(kz021), visit = as.factor(visit), 
    bullied_010 = as.factor(bullied_010), m_dep = as.factor(m_dep),
    psych7y = as.factor(psych7y), abuse_010 = as.factor(abuse_010),
    dv_010 = as.factor(dv_010), m_ed = as.factor(m_ed),
    agey = age / 12) %>% group_by(id) %>% filter(n() >= 2) %>% 
  mutate(id = cur_group_id()) %>% ungroup()

dat_F %>% group_by(id) %>% count() %>% ungroup() %>% 
  summarise(n_distinct(id), median(n), min(n), max(n), IQR(n))

dat_F$id <- as.factor(dat_F$id)

summary(dat_M)
summary(dat_F)

#------------------------------#
#### SAVE ANALYSIS DATASETS ####
#------------------------------#

save.image("alsp_dep_dat.RData")
