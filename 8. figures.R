
#---------------------#
#### LOAD PACKAGES ####
#---------------------#

library(tidyverse)
library(patchwork)
library(forestplot)
library(BlandAltmanLeh)
library(scales)
library(rlang)
library(sitar)
library(psme)

#------------------------------------------#
#### PLOT PREDICTED MEAN  AND SS CURVES ####
#------------------------------------------#

load("dep_psme_mods.RData")
load("dep_face_mods.RData")

xTraj <- function (model, new.x) {
  pop <- model$pcoef[[1]] + psme:::EvalSmooth(model$smooth[[1]], new.x)
  sub <- pop + psme:::EvalSmooth(model$smooth[[2]], new.x)
  list(pop = pop, sub = sub)
}

age.M <- age.F <- seq(10, 27, length.out = 200)

means_M <- data.frame(
  age = face_models$M$argvals.new,
  fpca_mean = face_models$M$mu.new,
  lme_mean = xTraj(psme_models$M, age.M)$pop,
  sex = "(A) Males") %>% 
  filter(age <= 26)

means_F <- data.frame(
  age = face_models$F$argvals.new,
  fpca_mean = face_models$F$mu.new,
  lme_mean = xTraj(psme_models$F, age.F)$pop,
  sex = "(B) Females") %>% 
  filter(age <= 26)

means_MF <- bind_rows(means_M, means_F)
rm(means_M, means_F)

#

load("dep_psme_mods.RData")
load("dep_face_mods.RData")

psme_pred_M <- psme_predictions$M %>% group_by(id) %>% filter(
  all(pred_dep >= 0 & pred_dep <= 26)) %>% ungroup() %>% 
  filter(age <= 26)

psme_pred_F <- psme_predictions$F %>% group_by(id) %>% filter(
  all(pred_dep >= 0 & pred_dep <= 26)) %>% ungroup() %>% 
  filter(age <= 26)

face_pred_M <- face_predictions$M  %>% group_by(id) %>% filter(
  all(pred_dep >= 0 & pred_dep <= 26)) %>% ungroup()  %>% 
  filter(age <= 26) %>% filter(
    age %in% unique(psme_pred_M$age) & id %in% unique(psme_pred_M$id))

face_pred_F <- face_predictions$F  %>% group_by(id) %>% filter(
  all(pred_dep >= 0 & pred_dep <= 26)) %>% ungroup()  %>% 
  filter(age <= 26) %>% filter(
    age %in% unique(psme_pred_F$age) & id %in% unique(psme_pred_F$id))

psme_pred_F <- psme_pred_F %>% filter(id %in% unique(face_pred_F$id))

#

graphics.off()
grDevices::cairo_pdf("res/Figure_1.pdf", height = 7, width = 5.5)
par(mfrow = c(3, 2), mar = c(4, 3.6, 2, 1), oma = c(1, 0, 0, 0), mgp = c(2.2, 0.8, 0))

# --- PANELS A & B: Mean Trajectories ---

plot_params <- list(
  list(data = means_MF[means_MF$sex == "(A) Males", ], title = '(A) Mean trajectory: males', y_label = 'Symptoms score', ylim = c(3, 8)),
  list(data = means_MF[means_MF$sex == "(B) Females", ], title = '(B) Mean trajectory: females', y_label = 'Symptoms score', ylim = c(3, 8))
)

for (params in plot_params) {
  plot(
    x = params$data$age, 
    y = params$data$fpca_mean, 
    type = "n",
    las = 1, 
    axes = F,
    ylim = params$ylim, 
    xlab = 'Age, y', 
    ylab = params$y_label
  )
  title(params$title, adj = 0, line = 0.7)
  axis(1)
  axis(2, las = 1)
  box(bty = "l")
  lines(params$data$age, params$data$fpca_mean, col = "skyblue", lwd = 2.5, lty = 1)
  lines(params$data$age, params$data$lme_mean, col = "salmon", lwd = 2.5, lty = 6)
  
  legend("bottomright", legend = c("FPCA", "PLME"), col = c("skyblue", "salmon"),
         lty = c(1, 6), lwd = 2, bty = "n", cex = 0.9)

}

# --- PANELS C, D, E, F: Individual Trajectories ---
plot_params <- list(
  list(data = face_pred_M, title = '(C) FPCA: all males', y_label = 'Symptoms score', ylim = c(0, 26)),
  list(data = psme_pred_M, title = '(D) PLME: all males', y_label = 'Symptoms score', ylim = c(0, 26)),
  list(data = face_pred_F, title = '(E) FPCA: all females', y_label = 'Symptoms score', ylim = c(0, 26)),
  list(data = psme_pred_F, title = '(F) PLME: all females', y_label = 'Symptoms score', ylim = c(0, 26))
)

for (params in plot_params) {
  mplot(
    x = age, 
    y = pred_dep, 
    id = id, 
    data = params$data, 
    col = id, 
    las = 1, 
    ylim = params$ylim, 
    xlab = 'Age, y', 
    ylab = params$y_label,
    axes = F
  )
  title(params$title, adj = 0, line = 0.6)
  
  axis(1)
  axis(2, las = 1)
  box(bty = "l")
}

dev.off()

rm(list = ls())

#-----------------------------------------#
#### PLOT OBSERVED-PREDICTED SS CURVES ####
#-----------------------------------------#

load("dep_psme_mods.RData")
load("dep_face_mods.RData")

# MALES

#dat_M %>% group_by(id) %>% filter(
#  any(agey < 15), any(agey >= 15)) %>%
#  distinct(id) %>% ungroup %>% sample_n(20) 
# id = 3799, 2807, 2801, 1121

psme_pred_M <- psme_predictions$M %>% filter(
  id == 2801 | id == 2807 | id == 3799 | id == 1121) %>% rename(LME_dep = pred_dep)

face_pred_M <- face_predictions$M %>% filter(age %in% unique(psme_pred_M$age)) %>% 
  filter(id == 2801 | id == 2807 | id == 3799 | id == 1121) %>% rename(FPCA_dep = pred_dep)

obs_pred_M <- dat_M %>% filter(id == "2801" | id == "2807" | id == "3799" | id == "1121") %>% 
  select(id, agey, dep) %>% rename(age = agey) %>% mutate(id = as.integer(id)) %>% 
  full_join(psme_pred_M) %>% full_join(face_pred_M) %>% mutate(
    sex = ifelse(id == 2801, "(A) Male 1", ifelse(
    id == 2807, "(B) Male 2", ifelse(
      id == 1121, "(C) Male 3", "(D) Male 4")))
    )

# FEMALES

#dat_F %>% group_by(id) %>% filter(
#  any(agey < 15), any(agey >= 15)) %>%
#  distinct(id) %>% ungroup %>% sample_n(20) 
# id = 2518, 2522, 3453, 2500

psme_pred_F <- psme_predictions$F %>% filter(
  id == 2518 | id == 2522 | id == 3453 | id == 2500) %>% rename(LME_dep = pred_dep)

face_pred_F <- face_predictions$F %>% filter(age %in% unique(psme_pred_F$age)) %>% 
  filter(id == 2518 | id == 2522 | id == 3453 | id == 2500) %>% rename(FPCA_dep = pred_dep)

obs_pred_F <- dat_F %>% filter(id == "2518" | id == "2522" | id == "3453" | id == "2500") %>% 
  select(id, agey, dep) %>% rename(age = agey) %>% mutate(id = as.integer(id)) %>% 
  full_join(psme_pred_F) %>% full_join(face_pred_F) %>% mutate(
    sex = ifelse(id == 2518, "(C) Female 1", ifelse(
      id == 2522, "(D) Female 2", ifelse(
        id == 3453, "(E) Female 3", "(F) Female 4")))
      )

##

obs_pred_MF <- bind_rows(obs_pred_M, obs_pred_F)

obs_pred_MF$sex <- factor(
  obs_pred_MF$sex, levels=c(
    "(A) Male 1",
    "(B) Male 2",
    "(C) Male 3",
    "(D) Male 4",
    "(C) Female 1",
    "(D) Female 2",
    "(E) Female 3",
    "(F) Female 4"
    
  ))

obs_pred_MF <- obs_pred_MF %>% filter(age <= 26)

graphics.off()
grDevices::cairo_pdf("res/Figure_2.pdf", height = 4.5, width = 9)

ggplot(obs_pred_MF, aes(x = age)) + theme_classic() + geom_line(
    aes(y = LME_dep, color = "PLME"), linewidth = 1, linetype = 6) + geom_line(
      aes(y = FPCA_dep, color = "FPCA"),  linewidth = 1) + geom_point(
        aes(y = dep, color = "Observed"), size = 1) + scale_y_continuous(
        labels = number_format(accuracy = 1)) + scale_color_manual(
          values = c("Observed" = "black", "PLME" = "salmon", "FPCA" = "skyblue"), 
          breaks = c("Observed", "FPCA", "PLME")) + labs(
            x = "Age, y", y = "Symptoms score", color = "Legend") + facet_wrap(
              . ~ sex, scales = "fixed", strip.position = "top", ncol = 4) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        strip.background = element_blank(), strip.text = element_text(face="bold", hjust = 0)) +
  guides(colour = guide_legend(override.aes = list(size = 4)))

dev.off()

rm(list = ls())

#--------------------------------------#
#### PLOT PREDICTED VELOCITY CURVES ####
#--------------------------------------#

load("dep_psme_mods.RData")
load("dep_face_mods.RData")

xTraj <- function (model, new.x) {
  pop <- model$pcoef[[1]] + psme:::EvalSmooth(model$smooth[[1]], new.x)
  sub <- pop + psme:::EvalSmooth(model$smooth[[2]], new.x)
  list(pop = pop, sub = sub)
}

age.M <- age.F <- seq(10, 27, length.out = 200)

means_M <- data.frame(
  age = face_models$M$argvals.new,
  fpca_mean = face_models$M$mu.new,
  lme_mean = xTraj(psme_models$M, age.M)$pop,
  sex = "(A) Males") %>% 
  filter(age <= 26)

means_F <- data.frame(
  age = face_models$F$argvals.new,
  fpca_mean = face_models$F$mu.new,
  lme_mean = xTraj(psme_models$F, age.F)$pop,
  sex = "(B) Females") %>% 
  filter(age <= 26)

means_MF <- bind_rows(means_M, means_F)
rm(means_M, means_F)

compute_mean_velocity <- function(df, var) {
  df %>% group_by(sex) %>% reframe(
    age = age,
    velocity = predict(smooth.spline(age, !!sym(var)), x = age, deriv = 1)$y) %>%
    group_by(age, sex) %>% summarize(mean_vel = mean(velocity), .groups = "drop")
}

lme_mvel <- compute_mean_velocity(means_MF, "lme_mean") %>% mutate(method = "PLME")
face_mvel <- compute_mean_velocity(means_MF, "fpca_mean") %>% mutate(method = "FPCA")

meanvel <- bind_rows(lme_mvel, face_mvel)

load("dep_psme_mods.RData")
load("dep_face_mods.RData")

psme_pred_M <- psme_predictions$M %>% group_by(id) %>% filter(
  all(pred_dep >= 0 & pred_dep <= 26)) %>% ungroup() %>% 
  filter(age <= 26)

psme_pred_F <- psme_predictions$F %>% group_by(id) %>% filter(
  all(pred_dep >= 0 & pred_dep <= 26)) %>% ungroup() %>% 
  filter(age <= 26)

face_pred_M <- face_predictions$M  %>% group_by(id) %>% filter(
  all(pred_dep >= 0 & pred_dep <= 26)) %>% ungroup()  %>% 
  filter(age <= 26) %>% filter(
    age %in% unique(psme_pred_M$age) & id %in% unique(psme_pred_M$id))

face_pred_F <- face_predictions$F  %>% group_by(id) %>% filter(
  all(pred_dep >= 0 & pred_dep <= 26)) %>% ungroup()  %>% 
  filter(age <= 26) %>% filter(
    age %in% unique(psme_pred_F$age) & id %in% unique(psme_pred_F$id))

psme_pred_F <- psme_pred_F %>% filter(id %in% unique(face_pred_F$id))

compute_deriv <- function(df, var) {
  df %>% select(id, age, !!sym(var)) %>%
    nest_by(id) %>% mutate(deriv = list({
      x <- data$age
      y <- data[[var]]
      ss <- smooth.spline(x, y)
      pred <- predict(ss, x = x, deriv = 1L)
      tibble(dX = pred$x, dY = pred$y)
    })) %>% select(id, deriv) %>% unnest(cols = deriv)
}

lme_vel_M <- compute_deriv(psme_pred_M, "pred_dep")
face_vel_M <- compute_deriv(face_pred_M, "pred_dep")
lme_vel_F <- compute_deriv(psme_pred_F, "pred_dep")
face_vel_F <- compute_deriv(face_pred_F, "pred_dep")

#

graphics.off()
grDevices::cairo_pdf("res/dep_vel.pdf", height = 7, width = 5.5)
par(mfrow = c(3, 2), mar = c(4, 3.6, 2, 1), oma = c(1, 0, 0, 0), mgp = c(2.2, 0.8, 0))

# --- PANELS A & B: Mean Velocity ---

plot_params <- list(
  list(data = meanvel[meanvel$sex == "(A) Males", ], title = '(A) Mean velocity: males'),
  list(data = meanvel[meanvel$sex == "(B) Females", ], title = '(B) Mean velocity: females')
)

for (params in plot_params) {
  plot(x = params$data$age, y = params$data$mean_vel, type = "n", 
       las = 1, axes = F, ylim = c(-1.2, 1.1), 
       xlab = 'Age, y', ylab = 'Velocity, score / y')
  
  title(params$title, adj = 0, line = 0.7)
  axis(1); axis(2, las = 1); box(bty = "l")
  
  fpca_data <- params$data[params$data$method == "FPCA", ]
  lme_data  <- params$data[params$data$method == "PLME", ]
  
  lines(fpca_data$age, fpca_data$mean_vel, col = "skyblue", lwd = 2.5, lty = 1)
  lines(lme_data$age,  lme_data$mean_vel,  col = "salmon",  lwd = 2.5, lty = 6)
  
  legend("bottomright", legend = c("FPCA", "PLME"), col = c("skyblue", "salmon"),
         lty = c(1, 6), lwd = 2, bty = "n", cex = 0.9)
}

# --- PANELS C, D, E, F: Individual velocity ---
plot_params <- list(
  list(data = face_vel_M, title = '(C) FPCA: all males', y_label = 'Velocity, score / y', ylim = c(-5, 6.5)),
  list(data = lme_vel_M, title = '(D) PLME: all males', y_label = 'Velocity, score / y', ylim = c(-5, 6.5)),
  list(data = face_vel_F, title = '(E) FPCA: all females', y_label = 'Velocity, score / y', ylim = c(-5, 6.5)),
  list(data = lme_vel_F, title = '(F) PLME: all females', y_label = 'Velocity, score / y', ylim = c(-5, 6.5))
)

for (params in plot_params) {
  mplot(
    x = dX, 
    y = dY, 
    id = id, 
    data = params$data, 
    col = id, 
    las = 1, 
    ylim = params$ylim, 
    xlab = 'Age, y', 
    ylab = params$y_label,
    axes = F
  )
  title(params$title, adj = 0, line = 0.6)
  
  axis(1)
  axis(2, las = 1)
  box(bty = "l")
}

dev.off()

rm(list = ls())

#-----------------------------------#
#### PLOT RESIDUALS AGAINST AGE  ####
#----====---------------------------#

#### FPCA

load("dep_face_mods.RData")

# MALES

face_resid_M <- face_models$M$newdata
face_resid_M$pred_dep <- face_models$M$y.pred
face_resid_M <- face_resid_M %>% rename(id = subj, agey = argvals) %>% 
  select(id, agey, pred_dep) %>% right_join(dat_M) %>% 
  select(id, agey, dep, pred_dep) %>% group_by(id) %>% mutate(
    resid = dep - pred_dep) %>% ungroup() %>% filter(
      agey >= 10 & agey <= 26)

# FEMALES

face_resid_F <- face_models$F$newdata
face_resid_F$pred_dep <- face_models$F$y.pred
face_resid_F <- face_resid_F %>% rename(id = subj, agey = argvals) %>% 
  select(id, agey, pred_dep) %>% right_join(dat_F) %>% 
  select(id, agey, dep, pred_dep) %>% group_by(id) %>% mutate(
    resid = dep - pred_dep) %>% ungroup() %>% filter(
      agey >= 10 & agey <= 26)

#### RESID: LME

xTraj <- function(model, new.x) {
  pop <- model$pcoef[[1]] + psme:::EvalSmooth(model$smooth[[1]], new.x)
  sub <- pop + psme:::EvalSmooth(model$smooth[[2]], new.x)
  list(pop = pop, sub = sub)
}

load("dep_psme_mods.RData")

make_preds_unique_age <- function(models, data_obs) {
  unique_ages <- unique(data_obs$agey)
  
  tmp <- as.data.frame(xTraj(models[[1]], unique_ages)$sub) %>% 
    mutate(agey = unique_ages) %>% pivot_longer(
      cols = starts_with("V"), names_to = "id_num", 
      values_to = "predicted")
  
  tmp$id <- as.integer(sub("^\\D+", "", tmp$id_num))
  
  obs_data <- data_obs %>% select(id, agey, observed = dep)
  
  final_df <- obs_data %>% mutate(id = as.integer(id)) %>% 
    left_join(tmp, by = c("id", "agey")) %>% select(
      id, agey, observed, predicted)
  
  return(final_df)
}

# MALES

psme_resid_M <- make_preds_unique_age(list(psme_models$M), dat_M)

psme_resid_M <- psme_resid_M %>% group_by(id) %>% mutate(
  resid = observed - predicted) %>% ungroup() %>% filter(
    agey >= 10 & agey <= 26)

# FEMALES

psme_resid_F <- make_preds_unique_age(list(psme_models$F), dat_F)

psme_resid_F <- psme_resid_F %>% group_by(id) %>%  mutate(
  resid = observed - predicted) %>% ungroup() %>% filter(
    agey >= 10 & agey <= 26)

#

summary(face_resid_M)
summary(psme_resid_M)

summary(face_resid_F)
summary(psme_resid_F)

#

graphics.off()
grDevices::cairo_pdf("res/resid.pdf", height = 7, width = 7)
par(mfrow = c(2, 2), mar = c(4, 3.6, 2, 1), oma = c(1, 0, 0, 0), mgp = c(2.2, 0.8, 0))

with(face_resid_M, plot(agey, resid, ylim = c(-14, 19), xlab = 'Age, y', ylab = 'residual', cex = 0.3))
title('(A) FPCA: males', adj = 0)

with(psme_resid_M, plot(agey, resid, ylim = c(-14, 19), xlab = 'Age, y', ylab = 'residual', cex = 0.3))
title('(B) PLME: males', adj = 0)

with(face_resid_F, plot(agey, resid, ylim = c(-14, 19), xlab = 'Age, y', ylab = 'residual', cex = 0.3))
title('(C) FPCA: females', adj = 0)

with(psme_resid_F, plot(agey, resid, ylim = c(-14, 19), xlab = 'Age, y', ylab = 'residual', cex = 0.3))
title('(D) PLME: females', adj = 0)

dev.off()

#----------------------#
#### PLOT PEAK DEP  ####
#----====--------------#

load("peak_dep_dat.RData")

colSums(!is.na(est_MF))

est_MF_long <- est_MF %>% select(
  sex, PeakDep_psme, PeakDepAge_psme, PeakDep_face, PeakDepAge_face,
  PVDep_psme, APVDep_psme, PVDep_face, APVDep_face) %>% drop_na() %>% select(
    sex, starts_with("PeakDep"), starts_with("PeakDepAge"), 
    starts_with("PVDep"), starts_with("APVDep")) %>% pivot_longer(
    cols = -sex, names_to = c(".value", "method"), names_sep = "_", values_drop_na = F) %>% 
  pivot_longer(cols = c(PeakDep, PeakDepAge, PVDep, APVDep), names_to = "variable", values_to = "value")

est_MF_long$variable <- dplyr::recode(
  est_MF_long$variable, 
  PeakDep = "(A) Peak symptoms score",
  PeakDepAge = "(B) Age at peak symptoms",
  PVDep = "(C) Peak symptoms velocity",
  APVDep = "(D) Age at peak velocity"
  
)

est_MF_long$method <- dplyr::recode(
  est_MF_long$method, 
  "psme" = "PLME",
  "face" = "FPCA"
)

est_MF_summ <- est_MF_long %>% 
  group_by(sex, method, variable) %>% 
  summarise_all(list(
    mean = ~mean(.,na.rm=T), 
    SD = ~sd(.,na.rm=T))) 

est_MF_summ$sex <- factor(est_MF_summ$sex, levels = c("Females", "Males"))
est_MF_summ$method <- factor(est_MF_summ$method, levels = c("PLME", "FPCA"))

(P1 <- est_MF_summ %>% filter(variable == "(A) Peak symptoms score") %>% ggplot() + 
    geom_point(data = est_MF_long[est_MF_long$variable == "(A) Peak symptoms score",],
               position = position_dodge(width = 0.5), aes(x = sex, y = value, group = method),
               size = 0.1, alpha = 0.1, color = "lightgray") + theme_classic() + geom_pointrange(
                 position = position_dodge(width = 0.5), size = 1, aes(
                   x = sex, y = mean, ymin = mean - SD, ymax = mean + SD, col = method,
                   group = method)) + coord_flip() + scale_color_manual(
                     values = c("PLME" = "salmon", "FPCA" = "skyblue")) +
    ggtitle("(A) Peak symptoms")+ ylab("Score") +
    theme(axis.title.y = element_blank(), legend.title = element_blank(), 
  plot.title = element_text(face="bold", size = 10)) +
  guides(col = guide_legend(override.aes = list(size = 0.8), reverse = T)))

(P2 <- est_MF_summ %>% filter(variable == "(B) Age at peak symptoms") %>% ggplot() + 
    geom_point(data = est_MF_long[est_MF_long$variable == "(B) Age at peak symptoms",],
               position = position_dodge(width = 0.5), aes(x = sex, y = value, group = method),
               size = 0.1, alpha = 0.1, color = "lightgray") + theme_classic() + geom_pointrange(
                 position = position_dodge(width = 0.5), size = 1, aes(
                   x = sex, y = mean, ymin = mean - SD, ymax = mean + SD, col = method,
                   group = method)) + coord_flip() + scale_color_manual(
                     values = c("PLME" = "salmon", "FPCA" = "skyblue")) +
    ggtitle("(B) Age at peak symptoms")+ ylab("Age, y") +
    theme(axis.title.y = element_blank(), legend.title = element_blank(),
       plot.title = element_text(face="bold", size = 10)) +
    guides(col = guide_legend(override.aes = list(size = 0.8), reverse = T)))

(P3 <- est_MF_summ %>% filter(variable == "(C) Peak symptoms velocity") %>% ggplot() + 
    geom_point(data = est_MF_long[est_MF_long$variable == "(C) Peak symptoms velocity",],
               position = position_dodge(width = 0.5), aes(x = sex, y = value, group = method),
               size = 0.1, alpha = 0.1, color = "lightgray") + theme_classic() + geom_pointrange(
                 position = position_dodge(width = 0.5), size = 1, aes(
                   x = sex, y = mean, ymin = mean - SD, ymax = mean + SD, col = method,
                   group = method)) + coord_flip() + scale_color_manual(
                     values = c("PLME" = "salmon", "FPCA" = "skyblue")) +
    ggtitle("(C) Peak velocity")+ ylab("Velocity, score / y") +
    theme(axis.title.y = element_blank(), legend.title = element_blank(), 
          plot.title = element_text(face="bold", size = 10)) +
    guides(col = guide_legend(override.aes = list(size = 0.8), reverse = T)))

(P4 <- est_MF_summ %>% filter(variable == "(D) Age at peak velocity") %>% ggplot() + 
    geom_point(data = est_MF_long[est_MF_long$variable == "(D) Age at peak velocity",],
               position = position_dodge(width = 0.5), aes(x = sex, y = value, group = method),
               size = 0.1, alpha = 0.1, color = "lightgray") + theme_classic() + geom_pointrange(
                 position = position_dodge(width = 0.5), size = 1, aes(
                   x = sex, y = mean, ymin = mean - SD, ymax = mean + SD, col = method,
                   group = method)) + coord_flip() + scale_color_manual(
                     values = c("PLME" = "salmon", "FPCA" = "skyblue")) +
    scale_y_continuous(breaks = c(12, 14, 16, 18)) + 
    ggtitle("(D) Age at peak velocity")+ ylab("Age, y") +
    theme(axis.title.y = element_blank(), legend.title = element_blank(),
          plot.title = element_text(face="bold", size = 10)) +
    guides(col = guide_legend(override.aes = list(size = 0.8), reverse = T)))

graphics.off()
grDevices::cairo_pdf("res/Figure_3.pdf", height = 5.5, width = 6)
par(mar = c(4, 3.6, 1.6, 0.5), oma = c(0, 4, 0, 0))

((P1 + P2) / (P3 + P4)) + plot_layout( 
  guides = "collect", axis_titles = "collect", axes = "collect") & 
  theme( legend.position = 'bottom') 

dev.off()

rm(list = ls())

#---------------------#
#### BAP PEAK DEP  ####
#---------------------#

load("peak_dep_dat.RData")

data_clean <- est_MF %>% select(
  sex, 
  PeakDep_face, PeakDepAge_face, PeakDep_psme, PeakDepAge_psme,
  PVDep_psme, APVDep_psme, PVDep_face, APVDep_face) %>% 
  drop_na() %>% mutate(sex = as.factor(sex))

calculate_ba_and_plot_data <- function(df, var_face, var_psme) {
  ba_stats <- bland.altman.stats(df[[var_face]], df[[var_psme]])
  
  plot_data <- data.frame(mean = ba_stats$means, diff = ba_stats$diffs)
  
  plot_data <- plot_data %>% mutate(
      is_outlier = case_when(diff > ba_stats$upper.limit ~ TRUE,
        diff < ba_stats$lower.limit ~ TRUE, TRUE ~ FALSE)
      )
  
  return(list(stats = ba_stats, data = plot_data))
}

ba_results <- data_clean %>% group_by(sex) %>% summarise(
    peak_dep_results = list(calculate_ba_and_plot_data(
      pick(everything()), "PeakDep_face", "PeakDep_psme")),
    peak_age_results = list(calculate_ba_and_plot_data(
      pick(everything()), "PeakDepAge_face", "PeakDepAge_psme")),
    pv_dep_results = list(calculate_ba_and_plot_data(
      pick(everything()), "PVDep_face", "PVDep_psme")),
    apv_results = list(calculate_ba_and_plot_data(
      pick(everything()), "APVDep_face", "APVDep_psme")),
    .groups = "drop" 
  )

#

plot_df <- ba_results %>% pivot_longer(
  cols = c(peak_dep_results, peak_age_results, pv_dep_results, apv_results), 
  names_to = "outcome", values_to = "results") %>% mutate(
    plot_data = map(results, "data"), outcome = recode(
      outcome, 
      "peak_dep_results" = "(A) Peak symptoms score", 
      "peak_age_results" = "(B) Age at peak symptoms",
      "pv_dep_results" = "(C) Peak symptoms velocity",
      "apv_results" = "(D) Age at peak velocity")) %>%
  select(sex, outcome, plot_data) %>%
  unnest(plot_data)

stats_df <- ba_results %>% pivot_longer(
  cols = c(peak_dep_results, peak_age_results, pv_dep_results, apv_results), 
  names_to = "outcome", values_to = "results") %>% mutate(
    outcome = recode(
      outcome, 
      "peak_dep_results" = "(A) Peak symptoms score", 
      "peak_age_results" = "(B) Age at peak symptoms",
      "pv_dep_results" = "(C) Peak symptoms velocity",
      "apv_results" = "(D) Age at peak velocity"),
    mean_diff = map_dbl(results, ~ .x$stats$mean.diffs),
    upr = map_dbl(results, ~ .x$stats$upper.limit),
    lwr = map_dbl(results, ~ .x$stats$lower.limit)) %>%
  select(sex, outcome, mean_diff, upr, lwr)

stats_df_P <- stats_df %>% filter(outcome == "(A) Peak symptoms score")
stats_df_AP <- stats_df %>% filter(outcome == "(B) Age at peak symptoms")
stats_df_PV <- stats_df %>% filter(outcome == "(C) Peak symptoms velocity")
stats_df_APV <- stats_df %>% filter(outcome == "(D) Age at peak velocity")

plot_df$sex <- factor(plot_df$sex, levels=c("Males", "Females")) 

#

(P1 <- plot_df %>% filter(outcome == "(A) Peak symptoms score") %>% ggplot(
  ., aes(x = mean, y = diff)) + theme_classic() + geom_point(
  aes(color = is_outlier), shape = 1) +
  geom_hline(data = stats_df_P, aes(yintercept = mean_diff), color = "black") +
  geom_hline(data = stats_df_P, aes(yintercept = upr), color = "red", linetype = "dashed", linewidth = 0.4) +
  geom_hline(data = stats_df_P, aes(yintercept = lwr), color = "red", linetype = "dashed", linewidth = 0.4) +
  scale_color_manual(values = c("FALSE" = "darkgray", "TRUE" = "darkred")) +
  facet_wrap(sex ~ ., scales = "free", strip.position = "top", ncol = 2) + labs(
    title = "(A) Peak symptoms score", x = "Means",
    y = "Differences") + theme(
      legend.position = "none",
      plot.title = element_text(face="bold", size = 10), strip.background = element_blank(), 
      strip.text = element_text(face="bold", hjust = 0))
)

(P2 <- plot_df %>% filter(outcome == "(B) Age at peak symptoms") %>% ggplot(
  ., aes(x = mean, y = diff)) + theme_classic() + geom_point(
    aes(color = is_outlier), shape = 1) +
    geom_hline(data = stats_df_AP, aes(yintercept = mean_diff), color = "black") +
    geom_hline(data = stats_df_AP, aes(yintercept = upr), color = "red", linetype = "dashed", linewidth = 0.4) +
    geom_hline(data = stats_df_AP, aes(yintercept = lwr), color = "red", linetype = "dashed", linewidth = 0.4) +
    scale_color_manual(values = c("FALSE" = "darkgray", "TRUE" = "darkred")) +
    scale_x_continuous(breaks = c(14, 15, 16, 17, 18, 19)) + 
    facet_wrap(sex ~ ., scales = "free", strip.position = "top", ncol = 2) + labs(
      title = "(B) Age at peak symptoms", x = "Means",
      y = "Differences") + theme(
        legend.position = "none",
        plot.title = element_text(face="bold", size = 10), strip.background = element_blank(), 
        strip.text = element_text(face="bold", hjust = 0))
)

(P3 <- plot_df %>% filter(outcome == "(C) Peak symptoms velocity") %>% ggplot(
  ., aes(x = mean, y = diff)) + theme_classic() + geom_point(
    aes(color = is_outlier), shape = 1) +
    geom_hline(data = stats_df_PV, aes(yintercept = mean_diff), color = "black") +
    geom_hline(data = stats_df_PV, aes(yintercept = upr), color = "red", linetype = "dashed", linewidth = 0.4) +
    geom_hline(data = stats_df_PV, aes(yintercept = lwr), color = "red", linetype = "dashed", linewidth = 0.4) +
    scale_color_manual(values = c("FALSE" = "darkgray", "TRUE" = "darkred")) +
    facet_wrap(sex ~ ., scales = "free", strip.position = "top", ncol = 2) + labs(
      title = "(C) Peak symptoms velocity", x = "Means",
      y = "Differences") + theme(
        legend.position = "none",
        plot.title = element_text(face="bold", size = 10), strip.background = element_blank(), 
        strip.text = element_text(face="bold", hjust = 0))
)

(P4 <- plot_df %>% filter(outcome == "(D) Age at peak velocity") %>% ggplot(
  ., aes(x = mean, y = diff)) + theme_classic() + geom_point(
    aes(color = is_outlier), shape = 1) +
    geom_hline(data = stats_df_APV, aes(yintercept = mean_diff), color = "black") +
    geom_hline(data = stats_df_APV, aes(yintercept = upr), color = "red", linetype = "dashed", linewidth = 0.4) +
    geom_hline(data = stats_df_APV, aes(yintercept = lwr), color = "red", linetype = "dashed", linewidth = 0.4) +
    scale_color_manual(values = c("FALSE" = "darkgray", "TRUE" = "darkred")) +
    scale_x_continuous(breaks = c(14, 15, 16, 17, 18, 19)) + 
    facet_wrap(sex ~ ., scales = "free", strip.position = "top", ncol = 2) + labs(
      title = "(D) Age at peak velocity", x = "Means",
      y = "Differences") + theme(
        legend.position = "none",
        plot.title = element_text(face="bold", size = 10), strip.background = element_blank(), 
        strip.text = element_text(face="bold", hjust = 0))
)

graphics.off()
grDevices::cairo_pdf("res/fig_BAP.pdf", height = 4.5, width = 9)
par(mar = c(4, 3.6, 1.6, 0.5), oma = c(0, 4, 0, 0))

((P1 | P2) / (P3 | P4)) + plot_layout( 
  guides = "collect", axis_titles = "collect", axes = "collect")

dev.off()

rm(list = ls())