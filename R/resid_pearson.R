# pearsonres: This is a function that is used to compute the conditional
# or marginal Pearson residules from the model.

#' @importFrom lme4 VarCorr

pearsonres <- function(model, cond = TRUE){

  X = model@pp$X
  n <- dim(X)[1]
  re_df <- as.data.frame(lme4::VarCorr(model))
  R <- (subset(re_df, re_df$grp == "Residual")$sdcor)^2 * diag(n)

  # Compute the conditional Pearson residuals
  if (cond == TRUE){

      res <- rawres(model, cond = TRUE) / sqrt(diag(R))

  # Compute the marginal Pearson residuals
  } else {

      Z = t(as.matrix(model@pp$Zt))
      p <- dim(X)[2]
      q <- dim(Z)[2]
      G <- (subset(re_df, re_df$grp != "Residual")$sdcor)^2 * diag(q)
      V = Z %*% G %*% t(Z) + R
      res <- as.vector(rawres(model, cond = FALSE) / sqrt(diag(V)))

  }

  # Return the computed residuals
  return(res)

}
