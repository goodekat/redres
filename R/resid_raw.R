# rawres: This is a function that is used to identify conditional or marginal raw
# residules from the model.

rawres <- function(model, cond = TRUE) {

  # Compute the conditional raw residuals
  if (cond == TRUE){
    # BLUPs <- coef(model)[[1]][,1]                          ## BlupS for each subject
    # BLUPS <- lme4::ranef(model)[[1]][,1]
    # subj <- table(unlist(model@flist[1]))                  ## number of levels for each subject
    # blups_y <- rep(BLUPs, c(as.numeric(as.matrix(subj))))  ## BLUP for each observation
    res <- as.vector(model@resp$y - predict(model))        ## conditional residule

  # Compute the marginal raw residuals
  } else {
    #res <- as.vector(model@resp$y - (model@pp$X %*% matrix(model@beta, ncol = 1)))
    res <- as.vector(model@resp$y - predict(model, re.form = NA))
  }

  # Return the computed residuals
  return(res)

}
