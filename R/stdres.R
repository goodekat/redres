
#' @importFrom lme4 VarCorr
#
# library(lme4)
#
# model <- lme4::lmer(heartrate ~ depth + duration + (1|bird),
#                     data = ggResidpanel::penguins)
#
# # Function for computing standardized residuals
# stdres <- function(model){
#
#   X = model@pp$X
#   Z = t(as.matrix(model@pp$Zt))
#
#   n <- dim(X)[1]
#   p <- dim(X)[2]
#   q <- dim(Z)[2]
#
#   re_df <- as.data.frame(lme4::VarCorr(model))
#   G <- (subset(re_df, re_df$grp != "Residual")$sdcor)^2 * diag(q)
#   R <- (subset(re_df, re_df$grp == "Residual")$sdcor)^2 * diag(n)
#
#   V = Z %*% G %*% t(Z) + R
#   Q = X %*% MASS::ginv(t(X) %*% solve(V) %*% X) %*% t(X)
#
#   var_rm = V-Q
#
#   rm <- resid(model, type = "response")
#
#   rstum <- rm / sqrt(var_rm)
#
#   return(rstum)
# }
#
# data.frame(model@frame[,1] - (X %*% matrix(model@beta, nrow = 3)),
#            resid(model, type = "response"))

