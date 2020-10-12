# Model results metrics for FairML manuscript
# Ayoub Bagheri, Laura Boeschoten, Erik-Jan van Kesteren, Daniel Oberski
# last edited 2020-02-04 by @vankesteren
# License: CC-BY 4.0 MSDSlab
results_metrics <- function(base_fit, trgt_fit) {
  tibble(
    "dFit"   = delta_fit(base_fit, trgt_fit),
    "dGamma" = delta_gamma(base_fit, trgt_fit),
    "dBeta"  = delta_beta(base_fit, trgt_fit),
    "dDelta" = delta_delta(base_fit, trgt_fit),
    "FPRd"   = delta_false_positive(trgt_fit),
    "FNRd"   = delta_false_negative(trgt_fit),
    "parity" = statistical_parity(trgt_fit)
  )
}

# Difference in log-likelihood
delta_fit <- function(base_fit, trgt_fit) {
  # fit_base
  ll_base <- logLik(base_fit)
  # fit_target
  ll_trgt  <- logLik(trgt_fit)
  
  return(c(ll_base - ll_trgt))
}

# difference intercept difference between groups
delta_gamma <- function(base_fit, trgt_fit) {
  pe_base <- parameterestimates(base_fit)
  gamma_base <- pe_base[pe_base$label == "ilb",]$est

  pe_trgt <- parameterestimates(trgt_fit)
  gamma_trgt <- pe_trgt[pe_trgt$label == "ilb",]$est
  
  return(gamma_base - gamma_trgt)
}

# norm of the change in coefficients
delta_beta <- function(base_fit, trgt_fit) {
  pe_base <- parameterestimates(base_fit)
  beta_base <- pe_base[pe_base$lhs == "health" & pe_base$op == "~",]$est
  
  pe_trgt <- parameterestimates(trgt_fit)
  beta_trgt <- pe_trgt[pe_trgt$lhs == "health" & pe_trgt$op == "~",]$est
  
  return(c(sqrt(crossprod(beta_base - beta_trgt))))
}

# norm of the DIF parameters
delta_delta <- function(base_fit, trgt_fit) {
  pe_base <- parameterestimates(base_fit)
  delta_base <- pe_base[grep("di[0-9]+", pe_base$label),]$est
  
  pe_trgt <- parameterestimates(trgt_fit)
  delta_trgt <- pe_trgt[grep("di[0-9]+", pe_trgt$label),]$est
  
  return(c(sqrt(crossprod(delta_base - delta_trgt))))
}

delta_false_positive <- function(trgt_fit) {
  # first, get the residual variance of the latent variables
  pe <- parameterestimates(trgt_fit)
  sigmas <- sqrt(pe[pe$lhs == pe$rhs & pe$lhs == "health",]$est)
  
  # then, get predicted risk per person
  # TODO: Marginalize predictions (weighted predictions by group size)
  # DONE: This is automatically done by constraining the beta parameters to be
  # equal across groups
  betas <- pe[pe$lhs == "health" & pe$op == "~", ]
  
  preds <- vector("list", trgt_fit@Model@ngroups)
  for (g in 1:trgt_fit@Model@ngroups) {
    # get betas for group g
    betas_g <- betas[betas$group == g,]
    
    # get X matrix for group g
    var_idx <- sapply(betas_g$rhs, function(nm) 
      which(trgt_fit@Data@ov.names[[g]] == nm)
    )
    mat_g <- trgt_fit@Data@X[[g]][, var_idx]
    
    # create risk pred for group g
    preds[[g]] <- mat_g %*% betas_g$est
  }
  
  # get cutoff (paper uses 0.55)
  cutoff <- quantile(unlist(preds), 0.55)
  
  # get Expected false positives
  efprs <- vector("list", trgt_fit@Model@ngroups)
  for (g in 1:trgt_fit@Model@ngroups) {
    # efpr = area of the prediction interval larger than cutoff for observations
    # with a mean predicted risk below the cutoff
    efprs[[g]] <- 1 - pnorm(cutoff, preds[[g]][preds[[g]] < cutoff], sigmas[g])
  }
  
  # if group 2 (black) has a higher efpr, then this number is negative:
  return(mean(efprs[[1]]) - mean(efprs[[2]]))
}

delta_false_negative <- function(trgt_fit) {
  # first, get the residual variance of the latent variables
  pe <- parameterestimates(trgt_fit)
  sigmas <- sqrt(pe[pe$lhs == pe$rhs & pe$lhs == "health",]$est)
  
  # then, get predicted risk per person
  # TODO: Marginalize predictions (weighted predictions by group size)
  # DONE: This is automatically done by constraining the beta parameters to be
  # equal across groups
  betas <- pe[pe$lhs == "health" & pe$op == "~", ]
  
  preds <- vector("list", trgt_fit@Model@ngroups)
  for (g in 1:trgt_fit@Model@ngroups) {
    # get betas for group g
    betas_g <- betas[betas$group == g,]
    
    # get X matrix for group g
    var_idx <- sapply(betas_g$rhs, function(nm) 
      which(trgt_fit@Data@ov.names[[g]] == nm)
    )
    mat_g <- trgt_fit@Data@X[[g]][, var_idx]
    
    # create risk pred for group g
    preds[[g]] <- mat_g %*% betas_g$est
  }
  
  # get cutoff (paper uses 0.55)
  cutoff <- quantile(unlist(preds), 0.55)
  
  # get Expected false negative rate
  efnrs <- vector("list", trgt_fit@Model@ngroups)
  for (g in 1:trgt_fit@Model@ngroups) {
    # efnr = area of the prediction interval smaller than cutoff for observations
    # with a mean predicted risk above the cutoff
    efnrs[[g]] <- pnorm(cutoff, preds[[g]][preds[[g]] > cutoff], sigmas[g])
  }
  
  # if group 2 (black) has a higher efnr, then this number is negative:
  return(mean(efnrs[[1]]) - mean(efnrs[[2]]))
}


# metric for unconditional statistical parity
# Verma (2017): A classifier satisfies statistical parity if subjects in both 
# protected and unprotected groups have equal probability of being assigned 
# to the positive predicted class.
statistical_parity <- function(trgt_fit) {
  # first, get risk score
  risk_score <- create_risk_score(trgt_fit)
  cutoff <- quantile(unlist(risk_score), 0.55)
  
  # probability to be assigned to the positive predicted class
  p_positive <- lapply(risk_score, function(pred) {
    sum(pred > cutoff) / length(pred)
  })
  
  # this is the metric, positive means black is more likely, negative means
  # white is more likely
  diff(unlist(p_positive))
}


create_risk_score <- function(trgt_fit) {
  pe <- parameterestimates(trgt_fit)
  betas <- pe[pe$lhs == "health" & pe$op == "~", ]
  preds <- vector("list", trgt_fit@Model@ngroups)
  for (g in 1:trgt_fit@Model@ngroups) {
    # get betas for group g
    betas_g <- betas[betas$group == g,]
    
    # get X matrix for group g
    var_idx <- sapply(betas_g$rhs, function(nm) 
      which(trgt_fit@Data@ov.names[[g]] == nm)
    )
    mat_g <- trgt_fit@Data@X[[g]][, var_idx]
    
    # create risk pred for group g
    preds[[g]] <- mat_g %*% betas_g$est
  }
  preds
}

