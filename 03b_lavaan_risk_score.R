# Algorithm risk scores from lavaan object using the MIMIC model
# Ayoub Bagheri, Laura Boeschoten, Erik-Jan van Kesteren, Daniel Oberski
# last edited 2020-03-06 by @vankesteren
# License: CC-BY 4.0 MSDSlab

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
