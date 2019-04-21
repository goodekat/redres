#' Compute the Residuals
#' @description
#' function to compute residuals. We we have four types methods to caculate residuals,
#' which are pearson residuals, generalized residuals, standardized residuals and
#' conditional residuals.
#' @usage redres(model, type = "raw_cond")
#' @param model the model we fit from lmer function in lme4 package.
#' @param type  type of residuals. we have generalized, standardized, conditional and pearson residuals.
#'
#' @importFrom assertthat assert_that
#' @export
#' @return return residuals according to different types.
#'
#' @examples
#' # fit the model
#' library(lme4)
#' fm1 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#' # compute the residuals
#' redres(fm1, type = "raw_cond")
#' # change the residual type
#' redres(fm1, type = "std_mar")

redres <- function(model, type = "raw_cond"){

  assertthat::assert_that(is.character(type), msg = 'type must be a string.')
  if(class(model)[1] != "lmerMod"){
    stop("The input model type is not accepted by plot_genres. Model must be fit using 'lmer'.")
  }

  if(type=="raw_cond"){
    result <- rawres(model, cond = TRUE)
  }
  else if(type=="raw_mar"){
    result <- rawres(model, cond = FALSE)
  }
  else if(type=="pearson_cond"){
    result <- pearsonres(model, cond = TRUE)
  }
  else if(type=="pearson_mar"){
    result <- pearsonres(model, cond = FALSE)
  }
  else if(type=="std_cond"){
    result <- stdres(model, cond = TRUE)
  }
  else if(type=="std_mar"){
    result <- stdres(model, cond = FALSE)
  }
  else if(type=="genres"){
    result <- genres(model)
  }
  else{
  print("This type is not included in our method, please input a correct one.")
  }

  # Return the residuals
  return(result)

}

