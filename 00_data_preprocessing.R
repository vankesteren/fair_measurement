# Data loading and preprocessing function for SafeML manuscript
# Ayoub Bagheri, Laura Boeschoten, Erik-Jan van Kesteren, Daniel Oberski
# last edited 2020-03-06 by @vankesteren
# License: CC-BY 4.0 MSDSlab

# Data was obtained from https://gitlab.com/labsysmed/dissecting-bias

load_data <- function() {
  # Data should be stored relative to the project file.
  df <- read_csv("data/data_new.csv")
  
  # Data preprocessing
  # Differences with paper:
  # We rescale some variables to obtain better model fit
  # No 0 imputation in cre_mean_log
  # log-transform of the cost_t and cost_avoidable_t variables
  # No dichotomization in bps_mean_t, and no subsequent 0 imputations
  df_lav <- df %>% 
    mutate(
      # indicators
      cre_mean_log         = log10(cre_mean_t),
      cost_t_log           = log10(cost_t + 1),#/1e4,
      cost_avoidable_t_log = log10(cost_avoidable_t + 1),#/1e4,
      ldl_mean_t           = ldl_mean_t/20,
      bps_mean_t           = bps_mean_t/10,
      hct_mean_t           = hct_mean_t/10,
      
      # predictors
      cost_emergency_tm1        = cost_emergency_tm1/1e4,
      cost_dialysis_tm1         = cost_dialysis_tm1/1e4,
      cost_home_health_tm1      = cost_home_health_tm1/1e4,
      cost_ip_medical_tm1       = cost_ip_medical_tm1/1e4,
      cost_ip_surgical_tm1      = cost_ip_surgical_tm1/1e4,
      cost_laboratory_tm1       = cost_laboratory_tm1/1e4,
      cost_op_primary_care_tm1  = cost_op_primary_care_tm1/1e4,
      cost_op_specialists_tm1   = cost_op_specialists_tm1/1e4,
      cost_op_surgery_tm1       = cost_op_surgery_tm1/1e4,
      cost_other_tm1            = cost_other_tm1/1e4,
      cost_pharmacy_tm1         = cost_pharmacy_tm1/1e4,
      cost_physical_therapy_tm1 = cost_physical_therapy_tm1/1e4,
      cost_radiology_tm1        = cost_radiology_tm1/1e4
    )
  
  # split into train / test
  train_size <- floor(0.8 * nrow(df_lav))
  train_ind  <- sample(seq_len(nrow(df_lav)), size = train_size)
  
  return(list(
    full  = df_lav,
    train = df_lav[train_ind, ],
    test  = df_lav[-train_ind, ]
  ))
}

