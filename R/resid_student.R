
#' @importFrom lme4 VarCorr
#' @importFrom MASS ginv

# Function for computing studentized residuals
stdres <- function(model, cond = TRUE) {

  X = model@pp$X                                                    # model matrix
  Z = t(as.matrix(model@pp$Zt))                                     # design matrix
  n <- dim(X)[1]                                                    # number of observations
  p <- dim(X)[2]                                                    # number of fixed effects
  q <- dim(Z)[2]                                                    # number of random effects
  re_df <- as.data.frame(lme4::VarCorr(model))                      # data frame with random effect components and std dev
  G <- (subset(re_df, re_df$grp != "Residual")$sdcor)^2 * diag(q)   # variance matrix for gamma
  R <- (subset(re_df, re_df$grp == "Residual")$sdcor)^2 * diag(n)   # variance matrix for epsilon
  V = Z %*% G %*% t(Z) + R                                          # variance matrix for Y
  Q = round(X %*% MASS::ginv(t(X) %*% solve(V) %*% X) %*% t(X), 5)  # used for the residual variances

  # Compute the conditional studentized residuals
  if (cond == TRUE){

    K = diag(n) - Z %*% G %*% t(Z) %*% solve(V)
    var_rc <- diag(K %*% (V - Q) %*% t(K))
    rc <- rawres(model, cond = TRUE)
    res <- as.vector(rc / sqrt(var_rc))

  # Compute the marginal studentized residuals
  } else {

    var_rm = diag(V - Q)
    rm <- rawres(model, cond = FALSE)
    res <- as.vector(rm / sqrt(var_rm))

  }

  # Return the computed residuals
  return(res)

}
