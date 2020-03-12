# MIMIC model syntax for lavaan
# Ayoub Bagheri, Laura Boeschoten, Erik-Jan van Kesteren, Daniel Oberski
# last edited 2020-03-06 by @vankesteren
# License: CC-BY 4.0 MSDSlab

#-----------------------------------------------
# MIMIC model: assume race as group, name params.
# -----------------------------------------------
# In this section we create a base MIMIC model in
# lavaan where we name the parameters per group.
# These parameter names will be used later to 
# apply / release cross-group constraints.

# Measurement part of the MIMIC model
.measurement_part <- "
# -------------------------------------
# Measurement part: multiple indicators
# -------------------------------------
health =~ c(l0w, l0b)*gagne_sum_t   + c(l1w, l1b)*bps_mean_t + 
          c(l2w, l2b)*ghba1c_mean_t + c(l3w, l3b)*hct_mean_t + 
          c(l4w, l4b)*cre_mean_log  + c(l5w, l5b)*ldl_mean_t + 
          c(l6w, l6b)*cost_t_log    + c(l7w, l7b)*cost_avoidable_t_log
          
# Indicator intercepts
gagne_sum_t          ~ c(i0w, i0b)*1
bps_mean_t           ~ c(i1w, i1b)*1
ghba1c_mean_t        ~ c(i2w, i2b)*1
hct_mean_t           ~ c(i3w, i3b)*1
cre_mean_log         ~ c(i4w, i4b)*1
ldl_mean_t           ~ c(i5w, i5b)*1
cost_t_log           ~ c(i6w, i6b)*1
cost_avoidable_t_log ~ c(i7w, i7b)*1

# Latent intercept
health ~ c(ilw, ilb)*1
"

# Multiple cause part of the model. Variables from t-1 from the electronic 
# health record of the patient.
# These predictors were chosen based on a LASSO model using glmnet. See the file 
# "risk_score_variable_selection.R" for that model.
.causes_part <- "
# --------------------------------
# Structural part: multiple causes
# --------------------------------
health ~ 
  `dem_age_band_18-24_tm1` + `dem_age_band_25-34_tm1` + 
  `dem_age_band_35-44_tm1` + `dem_age_band_45-54_tm1` + 
  `dem_age_band_55-64_tm1` + `dem_age_band_65-74_tm1` + 
  `dem_age_band_75+_tm1` + arrhythmia_elixhauser_tm1 + 
  arthritis_elixhauser_tm1 + bloodlossanemia_elixhauser_tm1 + 
  coagulopathy_elixhauser_tm1 + depression_elixhauser_tm1 + 
  electrolytes_elixhauser_tm1 + hypertension_elixhauser_tm1 + 
  neurodegen_elixhauser_tm1 + psychosis_elixhauser_tm1 + 
  renal_elixhauser_tm1 + uncompdiabetes_elixhauser_tm1 + 
  chf_romano_tm1 + dementia_romano_tm1 + hemiplegia_romano_tm1 + 
  hivaids_romano_tm1 + pulmonarydz_romano_tm1 + tumor_romano_tm1 + 
  ulcer_romano_tm1 + cost_dialysis_tm1 + cost_emergency_tm1 + 
  cost_home_health_tm1 + cost_ip_medical_tm1 + cost_ip_surgical_tm1 + 
  cost_laboratory_tm1 + cost_op_primary_care_tm1 + cost_op_specialists_tm1 + 
  cost_op_surgery_tm1 + cost_other_tm1 + cost_pharmacy_tm1 + 
  cost_physical_therapy_tm1 + cost_radiology_tm1 + cre_tests_tm1 + 
  `cre_min-low_tm1` + `cre_min-high_tm1` + `cre_max-high_tm1` + 
  `cre_max-normal_tm1` + `crp_min-normal_tm1` + `esr_min-normal_tm1` + 
  `esr_mean-high_tm1` + `esr_max-normal_tm1` + `hct_min-low_tm1` + 
  `hct_mean-normal_tm1` + `hct_max-low_tm1` + `hct_max-normal_tm1` + 
  `ldl_min-high_tm1` + `ldl_min-normal_tm1` + `ldl_max-high_tm1` + 
  `ldl_max-normal_tm1` + `sodium_min-low_tm1` + `sodium_max-normal_tm1` + 
  `trig_min-high_tm1` + `trig_mean-low_tm1` + gagne_sum_tm1 + dem_female
"


# Constraint part of the model. In the baseline model, all parameters are 
# constrained across groups, except the latent intercept.
.constraints_part <- "
# ---------------------------------
# Cross-group parameter constraints
# ---------------------------------
# Constrained intercepts
i0w == i0b
i1w == i1b
i2w == i2b
i3w == i3b
i4w == i4b
i5w == i5b
i6w == i6b
i7w == i7b

# Constrained slopes
l0w == 1 # for identification
l0w == l0b
l1w == l1b
l2w == l2b
l3w == l3b
l4w == l4b
l5w == l5b
l6w == l6b
l7w == l7b

# Constrain w latent intercept to 0
ilw == 0

# Differences intercepts
di0 := i0w - i0b
di1 := i1w - i1b
di2 := i2w - i2b
di3 := i3w - i3b
di4 := i4w - i4b
di5 := i5w - i5b
di6 := i6w - i6b
di7 := i7w - i7b

# Differences slopes
dl0 := l0w - l0b
dl1 := l1w - l1b
dl2 := l2w - l2b
dl3 := l3w - l3b
dl4 := l4w - l4b
dl5 := l5w - l5b
dl6 := l6w - l6b
dl7 := l7w - l7b
"

baseline_syntax <- function() {
  paste(.measurement_part, .causes_part, .constraints_part)
}

# Additionally, it is useful to have a function which releases the constrained
# intercepts for a set of target variables (numbers) in the constraint string.
release <- function(constraints, tgt_i) {
  # recursion if releasing multiple constraints
  if (length(tgt_i) > 1) {
    for (i in 1:length(tgt_i)) {
      constraints <- release(constraints, tgt_i[i])
    }
    return(constraints)
  }
  pttrn <- sprintf("i%1$sw == i%1$sb", tgt_i)
  return(str_replace(constraints, pttrn, paste("#", pttrn)))
}
