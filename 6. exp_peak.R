
#---------------------#
#### LOAD PACKAGES ####
#---------------------#

library(tidyverse)
library(emmeans)
library(metafor)
library(broom) 

#-----------------------#
#### LOAD & PREP DAT #### 
#-----------------------# 

load("peak_dep_dat.RData")

est_MF <- est_MF %>% select(
  id, sex, m_ed, m_dep, psych7y, abuse_010, dv_010, bullied_010, 
  PeakDep_face, PeakDepAge_face, PVDep_face, APVDep_face, c645a,
  PeakDep_psme, PeakDepAge_psme, PVDep_psme, APVDep_psme) %>% mutate(
    PeakDep_av = (PeakDep_face + PeakDep_psme) / 2,
    PeakDepAge_av = (PeakDepAge_face + PeakDepAge_psme) / 2,
    PVDep_av = (PVDep_face + PVDep_psme) / 2,
    APVDep_av = (APVDep_face + APVDep_psme) / 2,
    sex = ifelse(sex == "Males", 0, 1),
    m_ed = ifelse(m_ed == 0, 2, ifelse(m_ed == 2, 0, 1)),
    m_ed = as.factor(m_ed),
    sex = as.factor(sex))

str(est_MF)

dat_MF_face <- est_MF %>% filter(
  !is.na(PeakDep_face) & !is.na(PeakDepAge_face) & !is.na(PVDep_face) & !is.na(APVDep_face)) 
dat_MF_psme <- est_MF %>% filter(
  !is.na(PeakDep_psme) & !is.na(PeakDepAge_psme) & !is.na(PVDep_psme) & !is.na(APVDep_psme)) 
dat_MF_av <- est_MF %>% filter(
  !is.na(PeakDep_av) & !is.na(PeakDepAge_av) & !is.na(PVDep_av) & !is.na(APVDep_av))

# CHECK Ns for the 3 sets of outcomes

dat_MF_face %>% group_by(sex) %>% summarise(n_distinct(id)) # 7,514 (FPCA)
dat_MF_psme %>% group_by(sex) %>% summarise(n_distinct(id)) # 6,451 (FPCA)
dat_MF_av %>% group_by(sex) %>% summarise(n_distinct(id)) # 6,137 (FPCA)

#------------------------------#
#### MAIN REGRESSION MODELS ####
#------------------------------# 

x <- c(
  "sex",
  "m_ed + sex",
  "m_dep + m_ed + sex",
  "psych7y + m_ed + sex",
  "abuse_010 + m_ed + sex",
  "dv_010 + m_ed + sex",
  "bullied_010 + m_ed + sex"
  )

y_df <- tibble(
  y = c(
    "PeakDep_face", "PeakDepAge_face", "PVDep_face", "APVDep_face",
    "PeakDep_psme", "PeakDepAge_psme", "PVDep_psme", "APVDep_psme", 
    "PeakDep_av", "PeakDepAge_av", "PVDep_av", "APVDep_av"
  ),
  dat = case_when(
    grepl("_face$", y) ~ "face",
    grepl("_psme$", y) ~ "psme",
    grepl("_av$", y)   ~ "av"
  )
)

res <- expand_grid(x = x, y_df) %>% mutate(
  model = pmap(list(x, y, dat), ~ {
    d <- switch(
      ..3,
      face = dat_MF_face,
      psme = dat_MF_psme,
      av   = dat_MF_av
    )
    
    m <- lm(as.formula(paste(..2, "~", ..1)), data = d)
    
    tibble(
      n = nobs(m),
      tidy(m, conf.int = TRUE)
    )
  }
  )
) %>% unnest(model) %>% select(
  -std.error, -statistic) %>% filter(
    term != "(Intercept)")


res$res <- paste0(
  round(res$estimate, 2), " (",
  round(res$conf.low, 2), " to ", 
  round(res$conf.high, 2), ")")

res %>% write.csv("res/exp_peak_res.csv", row.names = F)
  
#-----------------------------#
#### SEX INTERACTION: FPCA ####
#-----------------------------# 

outcomes <- c("PeakDep_face", "PeakDepAge_face", "PVDep_face", "APVDep_face")

terms <- c(
  "m_ed:sex",
  "m_dep:sex",
  "psych7y:sex",
  "abuse_010:sex",
  "dv_010:sex",
  "bullied_010:sex"
)

p_interactions <- expand_grid(
  outcome = outcomes, term = terms) %>% mutate(
    p_int = map2_dbl(outcome, term, ~ {
      x <- sub(":sex$", "", .y)
      rhs <- if (x == "m_ed") {
        "m_ed * sex"
      } else {
        paste("m_ed +", x, "* sex")
      }
      m <- lm(as.formula(paste(.x, "~", rhs)), data = dat_MF_face)
      drop1(m, test = "F")[.y, "Pr(>F)"]
    })
  )

p_interactions %>% write.csv("res/exp_peak_sexinpval.csv")

sig_int <- p_interactions %>% filter(p_int < 0.1) %>%
  mutate(exposure = sub(":sex$", "", term))

#

sex_specific_res <- expand_grid(
  sig_int,
  sex_level = unique(dat_MF_face$sex)
) %>% mutate(
    rhs = if_else(
      exposure == "m_ed",
      "m_ed",
      paste("m_ed +", exposure)
    ), 
    model = map2(
      outcome, sex_level,
      ~ {
        m <- lm(
          as.formula(paste(.x, "~", rhs[cur_group_id()])),
          data = dat_MF_face %>% filter(sex == .y)
        )
        tibble(
          n = nobs(m),
          tidy(m, conf.int = TRUE)
        )
      }
    )
  ) %>% select(-term) %>% unnest(model) %>% filter(term != "(Intercept)") %>%
  mutate(sex = ifelse(sex_level == 0, "Males", "Females")) %>% 
  select(-rhs, -std.error, -statistic, -sex_level)
  

emmeans_res <- sig_int %>%
  mutate(
    model = map2(
      outcome, exposure,
      ~ {
        rhs <- if (.y == "m_ed") {
          "m_ed * sex"
        } else {
          paste("m_ed +", .y, "* sex")
        }
        
        m <- lm(
          as.formula(paste(.x, "~", rhs)),
          data = dat_MF_face
        )
        
        emm <- emmeans(
          m,
          specs = c("sex", .y)
        )
        
        pairs(
          emm,
          reverse = TRUE
        ) %>%
          summary(infer = TRUE) %>%
          as_tibble()
      }
    )
  ) %>% unnest(model) %>% 
  select(-SE, -df, -t.ratio, -p.value) %>% filter(
    str_detect(contrast, "sex0.*- sex0|sex1.*- sex1"),
    str_detect(contrast, "-.*0$") 
  )

emmeans_res <- emmeans_res %>% mutate(
  res = paste0(
    round(estimate, 2), " (",
    round(lower.CL, 2), " to ",
    round(upper.CL, 2), ")"
  )
)

write.csv(
  emmeans_res,
  "res/exp_peak_res_sex.csv",
  row.names = F
)

rm(list = ls())
