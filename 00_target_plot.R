# Target plot function for SafeML manuscript
#
# This plot shows on the x-axis a percentile of the risk score, and on the y
# axis a mean summary of the target variable for each percentile.
# The size of the points is related to the number of observations in that 
# percentile. The plot is designed to be comparable to the plots in
# Obermeyer et al., 2019
#
# Ayoub Bagheri, Laura Boeschoten, Erik-Jan van Kesteren, Daniel Oberski
# last edited 2020-03-06 by @vankesteren
# License: CC-BY 4.0 MSDSlab

target_plot <- function(df_pred, risk_score_var, target_var, smooth = 1.5) {
  # tidyverse-style variable quosures
  target_var     <- enquo(target_var)
  risk_score_var <- enquo(risk_score_var)
  
  df_pred %>% 
    mutate(perc = ntile(!!risk_score_var, 100)) %>% 
    group_by(perc, race) %>% 
    summarize(
      value = mean(!!target_var, na.rm = TRUE),
      n = n()
    ) %>% 
    ggplot(aes(x = perc, y = value, colour = race, size = n)) +
    geom_point() +
    geom_smooth(se = FALSE, span = smooth) + 
    labs(
      x      = "Percentile of risk score",
      colour = "Race"
    ) +
    scale_colour_fira() + 
    scale_radius(guide = "none", trans = "log", range = c(1, 2)) +
    theme_fira() +
    theme(legend.position = "top")
}

