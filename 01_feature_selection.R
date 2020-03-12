# Risk score replication for variable selection for SafeML manuscript
# Ayoub Bagheri, Laura Boeschoten, Erik-Jan van Kesteren, Daniel Oberski
# last edited 2020-03-09 by @vankesteren
# License: CC-BY 4.0 MSDSlab
library(tidyverse)
library(firatheme)
library(glmnet)
source("./00_data_preprocessing.R")

set.seed(45)
df <- load_data()[["full"]] %>% 
  select(contains("tm1"), dem_female, race, risk_score_t)

# Create a LASSO model predicting the commercial risk score
# with cross-validated lambda value
X <- model.matrix(risk_score_t ~ ., data = df)
y <- log(c(df$risk_score_t) + 1)
mod_lasso_cv <- cv.glmnet(X, y)
mod_lasso    <- glmnet(X, y, lambda = mod_lasso_cv$lambda.1se)

# Create a replicated risk score and compare it to the original
risk_score_rep <- predict(mod_lasso, X)[,1]

# rank correlation
cor(risk_score_rep, df$risk_score_t, method = "spearman")

tibble(true = log(df$risk_score_t + 1), rep = risk_score_rep) %>% 
  ggplot(aes(y = true, x = rep)) +
  geom_point(size = 1.5, alpha = 1/50) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 0.5, lty = 2) +
  coord_fixed() +
  xlim(0, 6) +
  ylim(0, 6) +
  labs(y = "Commercial risk score (log-transformed)", 
       x = "Replicated risk score") +
  theme_fira()

firaSave("output/01_feature_selection.png", device = "png", dpi = 300,
         width = 5.5, height = 5.5)

# chosen predictors in the original dataset 
# (excluding race because it is special)
chosen_features <- 
  rownames(mod_lasso$beta)[mod_lasso$beta[,1] != 0] %>%
  .[. != "racewhite"] %>% 
  str_remove_all("`")

# write them to a file for future use
write_rds(chosen_features, "output/01_chosen_features.rds")
