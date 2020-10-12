# Algorithm risk scores with measurement model
# Ayoub Bagheri, Laura Boeschoten, Erik-Jan van Kesteren, Daniel Oberski
# last edited 2020-03-06 by @vankesteren
# License: CC-BY 4.0 MSDSlab

# load libraries & helper functions
library(tidyverse)
library(firatheme)
library(patchwork)
library(lavaan)
source("./00_data_preprocessing.R")
source("./00_target_plot.R")
source("./00_target_metric.R")
source("./03a_lavaan_syntax.R")
source("./03b_lavaan_risk_score.R")

# --------------------------------------------
# Data loading: full, train, and test datasets
# --------------------------------------------
set.seed(45) # set seed for reproducibility
df_list  <- load_data()
df_full  <- df_list$full
df_train <- df_list$train
df_test  <- df_list$test


# -----------------------
# Model Fitting
# -----------------------
# We start by fitting the baseline model: the model with all the constraints
mod_latent <- baseline_syntax()

# Fit the model using the lavaan function:
fit_latent <- lavaan(
  model          = mod_latent, 
  data           = df_train,
  auto.fix.first = FALSE,
  int.ov.free    = TRUE,
  int.lv.free    = TRUE,
  auto.var       = TRUE,
  information    = "observed",
  verbose        = TRUE,
  group          = "race",
  group.equal    = c("regressions", "residuals"), 
  missing        = "ML"
)

# The parameter estimates on the training data can be found using summary:
summary(fit_latent)

# We can also create a test fit object by applying these parameter 
# estimates to the test dataset. This gives us the out-of-sample 
# log-likelihood (fit).
test_fit_latent <- lavaan(
  model          = mod_latent, 
  data           = df_test,
  start          = fit_latent,
  do.fit         = FALSE,
  auto.fix.first = FALSE,
  int.ov.free    = TRUE,
  int.lv.free    = TRUE,
  auto.var       = TRUE,
  information    = "observed",
  verbose        = TRUE,
  group          = "race",
  group.equal    = c("regressions", "residuals"), 
  missing        = "ML",
  test           = "standard",
  optim.force.converged = TRUE
)

# save this model
test_fit_latent %>% write_rds("output/03_test_fit_latent.rds")


# ---------------------------------------
# Target plot to compare to other methods
# ---------------------------------------

# First, load in the old target plots
tgt_plot_commercial <- read_rds("output/02_tgt_plot_commercial.rds")
tgt_plot_replicated <- read_rds("output/02_tgt_plot_replicated.rds")
tgt_plot_parity     <- read_rds("output/02_tgt_plot_parity.rds")

# Then, create a target plot based on the measurement model
df_pred <- df_test
df_pred$risk_score_latent <- 0

risk_score_latent <- create_risk_score(test_fit_latent)
df_pred$risk_score_latent[test_fit_latent@Data@case.idx[[2]]] <- 
  risk_score_latent[[2]]
df_pred$risk_score_latent[test_fit_latent@Data@case.idx[[1]]] <- 
  risk_score_latent[[1]]

tgt_plot_latent <- df_pred %>% 
  target_plot(risk_score_latent, gagne_sum_t, 1) +
  labs(y = "Number of chronic conditions", 
       x = "Percentile of latent risk score")

(tgt_plot_replicated + ggtitle("A") +
 tgt_plot_parity     + ggtitle("B") +
 tgt_plot_latent     + ggtitle("C")) * 
  theme(legend.direction = "vertical") + 
  plot_layout(guides = "collect")

firaSave("output/03_measurement_model.pdf", width = 15, height = 6)

# -----------------------------------------
# Target metric to compare to other methods
# -----------------------------------------

# get all the risk scores
r_commercial <- df_pred$risk_score_t
r_replicated <- read_rds("output/02_risk_score_rep.rds")
r_corrected  <- read_rds("output/02_risk_score_par.rds")
r_latent     <- df_pred$risk_score_latent

c_conditions <- df_pred$gagne_sum_t
health_costs <- df_pred$cost_t_log

race         <- df_pred$race


tab <- tibble(
  risk_score = c("commercial", "replicated", "corrected", "latent"),
  disparity  = list(
    disparity_lin(c_conditions, r_commercial, race), 
    disparity_lin(c_conditions, r_replicated, race), 
    disparity_lin(c_conditions, r_corrected,  race), 
    disparity_lin(c_conditions, r_latent,     race)
  )
) %>% unnest_wider(disparity)

xtable::xtable(tab, digits = 3)
