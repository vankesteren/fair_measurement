# Differential item functioning model search for algorithm risk scores
# Ayoub Bagheri, Laura Boeschoten, Erik-Jan van Kesteren, Daniel Oberski
# last edited 2020-03-09 by @vankesteren
# License: CC-BY 4.0 MSDSlab

# load libraries & helper functions
library(tidyverse)
library(lavaan)
library(firatheme)
library(patchwork)
source("./00_data_preprocessing.R")
source("./00_target_plot.R")
source("./03a_lavaan_syntax.R")
source("./03b_lavaan_risk_score.R")
source("./04a_results_functions.R")


# --------------------------------------------
# Data loading: full, train, and test datasets
# --------------------------------------------
set.seed(45) # set seed for reproducibility
df_list  <- load_data()
df_full  <- df_list$full
df_train <- df_list$train
df_test  <- df_list$test


# -------------------------------
# Model estimation: DIF in cost_t
# -------------------------------
# load the baseline model to compare to
test_fit_latent <- read_rds("output/03_test_fit_latent.rds")

# generate the model with DIF in the cost intercept
mod_dif_cost <- baseline_syntax() %>% release(6)
fit_dif_cost <- lavaan(
  model          = mod_dif_cost, 
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

test_fit_dif_cost <- lavaan(
  model          = mod_dif_cost, 
  data           = df_test,
  start          = fit_dif_cost,
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

write_rds(fit_dif_cost, "output/04_fit_dif_cost")
write_rds(test_fit_dif_cost, "output/04_test_fit_dif_cost")

# -----------------------------------------------
# Inspect the differential item functioning model
# -----------------------------------------------

# improvement in overall model fit
lavTestLRT(test_fit_latent, test_fit_dif_cost)

# differential item functioning
(dif_param <- parameterestimates(fit_dif_cost) %>% filter(label == "di6"))


# -----------------------------------------
# Create target plots for the cost variable
# -----------------------------------------

# Then, create a target plot based on the measurement model
df_pred <- df_test
df_pred$risk_score_latent <- 0
df_pred$risk_score_dif <- 0

risk_score_latent <- create_risk_score(test_fit_latent)
df_pred$risk_score_latent[test_fit_latent@Data@case.idx[[2]]] <- 
  risk_score_latent[[2]]
df_pred$risk_score_latent[test_fit_latent@Data@case.idx[[1]]] <- 
  risk_score_latent[[1]]

risk_score_dif <- create_risk_score(test_fit_dif_cost)
df_pred$risk_score_dif[test_fit_dif_cost@Data@case.idx[[2]]] <- 
  risk_score_dif[[2]]
df_pred$risk_score_dif[test_fit_dif_cost@Data@case.idx[[1]]] <- 
  risk_score_dif[[1]]

# --------------------------------------------
# Estimate DIF for each proxy and create table
# --------------------------------------------

# create a list of fit objects with a DIF parameter in each proxy
dif_fits <- lapply(0:7, function(i) {
  mod_dif_cost <- baseline_syntax() %>% release(i)
  dif_fit <- lavaan(
    model          = mod_dif_cost, 
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
  dif_fit@external$syntax   <- mod_dif_cost
  dif_fit@external$released <- i
})

write_rds(dif_fits, "output/04_dif_fits.rds")

# get the dif parameter estimates and standard errors for each model
sapply(dif_fits, function(fit) {
  param_label <- paste0("di", fit@external$released)
  dif_param <- parameterestimates(fit) %>% 
    .[.$label == param_label,]
  return(dif_param)
}) %>% 
  t() %>% 
  as_tibble() %>% 
  unnest(cols = c(lhs, op, rhs, block, group, label, est, se, z, pvalue, 
                  ci.lower, ci.upper)) %>% 
  rownames_to_column() %>% 
  select(rowname, est, ci.lower, ci.upper, se, z, pvalue) %>% 
  set_names(c("Model No.", "DIF", "95% (lower)", "95% (upper)", "SE", "Z", 
              "p-value")) %>% 
  mutate(Indicator = c(
    "Number of active chronic conditions",
    "Mean blood pressure",
    "Diabetes severity (HbA1c)",
    "Anemia severity (hematocrit)",
    "Renal failure (creatinine)",
    "Cholesterol (mean LDL)",
    "Healthcare cost",
    "Avoidable healthcare cost"
  )) %>% 
  select(`Model No.`, Indicator, everything()) %>% 
  xtable(align = "rclcccccc", digits = 3)


