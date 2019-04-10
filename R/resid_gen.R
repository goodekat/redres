# Function for computing generalized residuals
# Note that this function depends on the following assumptions:
# 1) normality assumption
# 2) variance components are independent and identical distributioned normal with mean 0

#' @importFrom lme4 VarCorr
#' @importFrom stats integrate model.matrix

genres <- function(model){
  # pulling out response data
  y <- model@resp$y

  # pulling out data means
  fm <- model.matrix(model, type = "fixed")
  fb <- model@beta
  muhat <- fm%*%fb

  # pulling out variance components
  v2hat <- as.data.frame(lme4::VarCorr(model))
  tvar <- sum(v2hat$vcov) # summing over variance components

  # assumed distribution fitted to the data
  # assuming y ~ Normal(muhat, tvar)
  c <- (2*pi*tvar)^(-1/2)
  f_y <- function(x, m, v = tvar){
    c*exp(-(1/(2*v))*(x-m)^2)
  }

  # computing generalized residuals
  genr <- NULL
  for(i in 1:length(y)){
    genr[i] <- integrate(function(x) f_y(x, m = muhat[i]), -Inf, y[i])$value
  }

  return(genr)
}
