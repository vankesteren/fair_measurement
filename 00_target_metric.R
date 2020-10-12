#' Disparity function for Fair Inference on Error-Prone Outcomes
#' 
#' This function estimates conditional disparity on an outcome.
#' Conditional parity: p(Z = z | Y^ = y^ S = 0) == p(Z = z | Y^ = y^, S = 1), 
#' i.e. the conditional distribution of Z is the same for different levels of 
#' the sensitive feature S.
#' Under the assumptions of linear regression, disparity is the coefficient of 
#' S in the model z ~ y^ + s. This function computes this coefficient, its se 
#' and CI
#'
#' Ayoub Bagheri, Laura Boeschoten, Erik-Jan van Kesteren, Daniel Oberski
#' last edited 2020-10-06 by @vankesteren
#' License: CC-BY 4.0 MSDSlab

disparity_lin <- function(z, y_hat, s) {
  df <- data.frame(zs = z, ys = y_hat, s = s)
  fit <- lm(zs ~ s + ys, data = df)
  out <- c(coef(fit)[2], sqrt(diag(vcov(fit)))[2], confint(fit, 2))
  names(out) <- c("disparity", "se", "2.5%", "97.5%")
  out
}


disparity_np <- function(z, y_hat, s) {
  df <- data.frame(zs = c(scale(z)), ys = c(scale(y_hat)), s = s)
  fit <- mgcv::gam(zs ~ s + s(ys, bs = "cs"), data = df)
  fsm <- summary(fit)
  out <- c(fsm$p.coeff[2], fsm$se[2])
  out[3] <- out[1] - 1.96*out[2]
  out[4] <- out[1] + 1.96*out[2]
  names(out) <- c("disparity", "se", "2.5%", "97.5%")
  out
}

