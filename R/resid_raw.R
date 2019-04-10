# rawres: This is a function that is used to identify conditional or marginal raw
# residules from the model.

#' @importFrom stats predict

rawres <- function(model, cond = TRUE) {

  # Compute the conditional raw residuals
  if (cond == TRUE){
    res <- as.vector(model@resp$y - predict(model))
  # Compute the marginal raw residuals
  } else {
    res <- as.vector(model@resp$y - (model@pp$X %*% matrix(model@beta, ncol = 1)))
  }

  # Return the computed residuals
  return(res)

}
