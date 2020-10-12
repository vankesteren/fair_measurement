# Statistical conditional parity correction for race in predicting cost_t_log
# Ayoub Bagheri, Laura Boeschoten, Erik-Jan van Kesteren, Daniel Oberski
# last edited 2020-03-09 by @vankesteren
# License: CC-BY 4.0 MSDSlab
library(tidyverse)
library(firatheme)
library(patchwork)
source("./00_data_preprocessing.R")
source("./00_target_plot.R")
source("./00_target_metric.R")


# load training and test data
set.seed(45)
dat <- load_data()

# use the chosen predictors (01_feature_selection)
var_selection <- 
  read_rds("output/01_chosen_features.rds") %>%  
  c("race", "cost_t_log")

df_train <- dat[["train"]] %>% select_at(var_selection)
df_test  <- dat[["test"]]

# create a linear regression model
fit_lm <- lm(cost_t_log ~ ., df_train)

# Create risk score using X including race
df_test[["risk_score_replicated"]] <- predict(fit_lm, newdata = df_test)
df_test[["risk_score_replicated"]] %>% write_rds("output/02_risk_score_rep.rds")

# Conditional parity correction
# Verma (2017): A classifier satisfies statistical parity if subjects in both 
# protected and unprotected groups have equal probability of being assigned 
# to the positive predicted class.
# Create risk score assuming everyone has the same race
df_test_parity <- df_test %>% mutate(race = "black")
df_test[["risk_score_parity"]] <- predict(fit_lm, newdata = df_test_parity)
df_test[["risk_score_parity"]] %>% write_rds("output/02_risk_score_par.rds")


# Now we can investigate the effect of conditional parity correction on the 
# number of chronic conditions.


# Create plots to explain why parity is not met on chronic illnesses
A <- df_test %>% 
  target_plot(risk_score_replicated, cost_t, 1) +
  scale_y_log10(breaks = c(1000, 3000, 8000, 20000, 60000), lim = c(500, 62000)) +
  labs(x = "Percentile of risk score", 
       y = "Mean healthcare cost",
       title = "A")

B <- df_test %>% 
  target_plot(gagne_sum_t, cost_t, 1) +
  scale_y_log10(breaks = c(1000, 3000, 8000, 20000, 60000), lim = c(500, 62000)) +
  labs(x = "Chronic conditions", 
       y = "Mean healthcare cost",
       title = "B")


(A + B) * theme(legend.direction = "vertical") + plot_layout(guides = "collect")
firaSave("output/02_parity_explanation.pdf", width = 10, height = 5)

# We can also create three plots to validate the replication: 
# commercial, replicated, and parity-corrected
tgt_plot_commercial <- df_test %>% 
  target_plot(risk_score_t, gagne_sum_t, 1) +
  labs(y = "Mean number of chronic conditions", 
       x = "Percentile of commercial risk score")

tgt_plot_commercial %>% write_rds("output/02_tgt_plot_commercial.rds")
  

tgt_plot_replicated <- df_test %>% 
  target_plot(risk_score_replicated, gagne_sum_t, 1) +
  labs(y = "Number of chronic conditions", 
       x = "Percentile of replicated risk score")

tgt_plot_replicated %>% write_rds("output/02_tgt_plot_replicated.rds")


tgt_plot_parity <- df_test %>% 
  target_plot(risk_score_parity, gagne_sum_t, 1) +
  labs(y = "Number of chronic conditions", 
       x = "Percentile of parity-corrected risk score")

tgt_plot_parity %>% write_rds("output/02_tgt_plot_parity.rds")


# save the plot
tgt_plot_commercial + tgt_plot_replicated + tgt_plot_parity
firaSave("output/02_parity_correction.pdf", width = 15, height = 6)
