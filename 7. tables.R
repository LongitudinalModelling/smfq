
#---------------------#
#### LOAD PACKAGES ####
#---------------------#

library(tidyverse)

#-------------------------------#
#### DESCRIPTIVE: SMFQ & AGE ####
#-------------------------------#

dat_MF <- read.table(
  "dat/alsp_smfq_long.csv", header = T, sep = ",", na.strings = c("", "NA")) %>% 
  filter(!is.na(age) & !is.na(dep)) %>% mutate(
    kz021 = as.factor(kz021), , agey = age / 12) %>% group_by(id) %>% 
  filter(n() >= 2) %>% mutate(id = cur_group_id()) %>% ungroup()
dat_MF <- dat_MF %>% mutate(kz021 = ifelse(kz021 == 1, "Male", "Female"))

dat_MF %>% group_by(visit, kz021) %>% summarise(
  n_participants = n_distinct(id), mean_age = mean(agey, na.rm = TRUE), 
  sd_age = sd(agey, na.rm = TRUE), mean_dep = mean(dep, na.rm = TRUE), 
  sd_dep = sd(dep, na.rm = TRUE), .groups = 'drop') %>%
  mutate(across(starts_with("mean_"), ~round(., 2))) %>%
  mutate(across(starts_with("sd_"), ~round(., 2))) %>% mutate(
    'Age (SD)' = paste0(mean_age, " (", sd_age, ")"), 
    'SMFQ (SD)' = paste0(mean_dep, " (", sd_dep, ")")) %>% 
  select(visit, kz021, n_participants, 'Age (SD)', 'SMFQ (SD)') %>% pivot_wider(
    names_from = kz021, values_from = c(n_participants, 'Age (SD)', 'SMFQ (SD)'),
    names_sep = " - ") %>% select(visit, starts_with("n_participants - "), starts_with(
      "Age (SD) - "), starts_with("SMFQ (SD) - ")) %>% 
  write_csv("res/smfq_descr.csv")

rm(list = ls())

#-------------------------------#
#### MISSING DATA COMPARISON ####
#-------------------------------#

dat_MF <- read.table(
  "dat/alsp_smfq_long.csv", header = TRUE, 
  sep = ",", na.strings = c("", "NA")) %>% filter(!is.na(kz021)) %>% group_by(id) %>% 
  mutate(in_analysis = sum(!is.na(age) & !is.na(dep)) >= 2) %>% ungroup() %>%  mutate(
    kz021 = as.factor(kz021), visit = as.factor(visit), 
    bullied_010 = as.factor(bullied_010), m_dep = as.factor(m_dep),
    psych7y = as.factor(psych7y), abuse_010 = as.factor(abuse_010),
    dv_010 = as.factor(dv_010), m_ed = as.factor(m_ed), 
    imd_preg5 = as.factor(imd_preg5), agey = age / 12)

dat_MF %>% group_by(in_analysis) %>% summarise(N = n_distinct(id))

target_vars <- c(
  "kz021", "m_ed", "m_dep", "psych7y",
  "abuse_010", "dv_010", "bullied_010"
)

dat_MF %>% group_by(in_analysis) %>% pivot_longer(
    cols = all_of(target_vars), names_to = "variable", values_to = "category") %>%
  filter(!is.na(category)) %>% group_by(in_analysis, variable, category) %>%
  summarise(N = n_distinct(id), .groups = "drop_last") %>% mutate(
    `(%)` = N / sum(N) * 100) %>% ungroup() %>% arrange(
      variable, in_analysis, category) %>% print(n = 50)

rm(list = ls())

#------------------------------#
#### DESCRIPTIVE: PEAK DEP  ####
#--------====------------------#

load("peak_dep_dat.RData")

est_MF %>% group_by(sex) %>% summarise(
  pface_n = n_distinct(PeakDep_face), 
  pface_mean = mean(PeakDep_face, na.rm = TRUE), 
  pface_sd = sd(PeakDep_face, na.rm = TRUE), 
  apface_mean = mean(PeakDepAge_face, na.rm = TRUE), 
  apface_sd = sd(PeakDepAge_face, na.rm = TRUE), 
  ppsme_n = n_distinct(PeakDep_psme), 
  ppsme_mean = mean(PeakDep_psme, na.rm = TRUE), 
  ppsme_sd = sd(PeakDep_psme, na.rm = TRUE), 
  appsme_mean = mean(PeakDepAge_psme, na.rm = TRUE), 
  appsme_sd = sd(PeakDepAge_psme, na.rm = TRUE), 
  
  pvface_n = n_distinct(PVDep_face), 
  pvface_mean = mean(PVDep_face, na.rm = TRUE), 
  pvface_sd = sd(PVDep_face, na.rm = TRUE), 
  apvface_mean = mean(APVDep_face, na.rm = TRUE), 
  apvface_sd = sd(APVDep_face, na.rm = TRUE), 
  pvpsme_n = n_distinct(PVDep_psme), 
  pvpsme_mean = mean(PVDep_psme, na.rm = TRUE), 
  pvpsme_sd = sd(PVDep_psme, na.rm = TRUE), 
  apvpsme_mean = mean(APVDep_psme, na.rm = TRUE), 
  apvpsme_sd = sd(APVDep_psme, na.rm = TRUE), 
  .groups = 'drop') %>% 
  write_csv("res/peak_descr_UN.csv")

est_MF %>% select(
  sex, PeakDep_psme, PeakDepAge_psme, PeakDep_face, PeakDepAge_face,
  PVDep_psme, APVDep_psme, PVDep_face, APVDep_face) %>% drop_na() %>% 
  group_by(sex) %>% summarise(
  pface_n = n_distinct(PeakDep_face), 
  pface_mean = mean(PeakDep_face, na.rm = TRUE), 
  pface_sd = sd(PeakDep_face, na.rm = TRUE), 
  apface_mean = mean(PeakDepAge_face, na.rm = TRUE), 
  apface_sd = sd(PeakDepAge_face, na.rm = TRUE), 
  ppsme_n = n_distinct(PeakDep_psme), 
  ppsme_mean = mean(PeakDep_psme, na.rm = TRUE), 
  ppsme_sd = sd(PeakDep_psme, na.rm = TRUE), 
  appsme_mean = mean(PeakDepAge_psme, na.rm = TRUE), 
  appsme_sd = sd(PeakDepAge_psme, na.rm = TRUE), 
  
  pvface_n = n_distinct(PVDep_face), 
  pvface_mean = mean(PVDep_face, na.rm = TRUE), 
  pvface_sd = sd(PVDep_face, na.rm = TRUE), 
  apvface_mean = mean(APVDep_face, na.rm = TRUE), 
  apvface_sd = sd(APVDep_face, na.rm = TRUE), 
  pvpsme_n = n_distinct(PVDep_psme), 
  pvpsme_mean = mean(PVDep_psme, na.rm = TRUE), 
  pvpsme_sd = sd(PVDep_psme, na.rm = TRUE), 
  apvpsme_mean = mean(APVDep_psme, na.rm = TRUE), 
  apvpsme_sd = sd(APVDep_psme, na.rm = TRUE), 
  .groups = 'drop') %>% 
  write_csv("res/peak_descr_SN.csv")

rm(list = ls())

#------------------------------#
#### CORRELATION: PEAK DEP  ####
#--------====------------------#

load("peak_dep_dat.RData")

est_MF %>% select(
  id, sex, PeakDep_psme, PeakDepAge_psme, PeakDep_face, PeakDepAge_face, 
  PVDep_psme, APVDep_psme, PVDep_face, APVDep_face) %>% drop_na() %>% 
  group_by(sex) %>% summarise(N = n_distinct(id))

est_MF %>% select(
  sex, PeakDep_psme, PeakDepAge_psme, PeakDep_face, PeakDepAge_face, 
  PVDep_psme, APVDep_psme, PVDep_face, APVDep_face) %>% drop_na() %>% 
  group_by(sex) %>% summarize(
    peak_peakage_psme = cor(PeakDep_psme, PeakDepAge_psme),
    peak_peakage_face = cor(PeakDep_face, PeakDepAge_face),
    pv_apv_psme = cor(PVDep_psme, APVDep_psme),
    pv_apv_face = cor(PVDep_face, APVDep_face),
    peak_psme_face = cor(PeakDep_psme, PeakDep_face),
    peakage_psme_face = cor(PeakDepAge_psme, PeakDepAge_face),
    pv_psme_face = cor(PVDep_psme, PVDep_face),
    apv_psme_face = cor(APVDep_psme, APVDep_face),
    .groups = "drop") %>% pivot_longer(
      cols = -sex, names_to = "comparison", 
      values_to = "correlation") %>% 
  write_csv("res/peak_corr_full.csv")

rm(list = ls())

#------------------------------------#
#### DESCRIPTIVE: EXP_PEAK SAMPLE ####
#------------------------------------#

load("peak_dep_dat.RData")

dat_MF_face <- est_MF %>% select(
  id, sex, m_ed, m_dep, psych7y, abuse_010, dv_010, bullied_010, 
  PeakDep_face, PeakDepAge_face, PVDep_face, APVDep_face,
  PeakDep_psme, PeakDepAge_psme, PVDep_psme, APVDep_psme) %>% filter(
    !is.na(PeakDep_face) & !is.na(PeakDepAge_face) & !is.na(PVDep_face) & 
      !is.na(APVDep_face)) %>% mutate(sex = as.factor(sex))

dat_MF_face %>% summarise(n_distinct(id))
dat_MF_face %>% group_by(sex) %>% summarise(N = n_distinct(id))
dat_MF_face %>% select(PeakDep_face, PeakDepAge_face, PVDep_face, APVDep_face) %>% summarise_all(
  list(mean = ~round(mean(.,na.rm = T), 2), sd = ~round(sd(.,na.rm = T), 1)))
dat_MF_face %>% group_by(sex) %>% select(PeakDep_face, PeakDepAge_face, PVDep_face, APVDep_face) %>% summarise_all(
  list(mean = ~round(mean(.,na.rm = T), 2), sd = ~round(sd(.,na.rm = T), 2)))

target_vars <- c("m_ed", "m_dep", "psych7y", "abuse_010", "dv_010", "bullied_010")

summarize_logic <- function(df) {
  df %>% pivot_longer(cols = all_of(target_vars), names_to = "variable", values_to = "category") %>%
    group_by(variable, category, .add = TRUE) %>% 
    summarise(N = n_distinct(id), .groups = "drop_last") %>% mutate(
      `(%)` = ifelse(is.na(category), NA, (N / sum(N[!is.na(category)])) * 100)) %>%
    ungroup()
}

bind_rows(
  dat_MF_face %>% summarize_logic(),
  dat_MF_face %>% group_by(sex) %>% summarize_logic()
) %>% arrange(variable, sex, category) %>% 
  print(n = 60)

# Ns for main analysis

lapply(
  c("sex", "m_ed", "m_dep", "dv_010",
    "abuse_010", "bullied_010", "psych7y"), function(v) {
      dat_MF_face %>% filter(
        !is.na(sex), !is.na(.data[[v]]),
        if (v != "sex") !is.na(m_ed) else TRUE) %>%
        group_by(.data[[v]]) %>% summarise(
          N = n_distinct(id), .groups = "drop")
    })
