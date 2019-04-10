# rawres: This is a function that is used to identify conditional or marginal raw
# residules from the model.

rawres <- function(model, cond = TRUE) {

  # Compute the conditional raw residuals
  if (cond == TRUE){
    #BLUPs <- coef(model)[[1]][,1]                          ## BlupS for each subject
    #BLUPS <- ranef(model)[[1]][,1]
    #subj <- table(unlist(model@flist[1]))                  ## number of levels for each subject
    #blups_y <- rep(BLUPs, c(as.numeric(as.matrix(subj))))  ## BLUP for each observation
    blups_y <- as.vector(predict(model))
    res <- model@resp$y - blups_y                           ## conditional residule
    res

  # Compute the marginal raw residuals
  } else {
    res <- as.vector(model@resp$y - (model@pp$X %*% matrix(model@beta, ncol = 1)))
  }

  # Return the computed residuals
  return(res)

}
