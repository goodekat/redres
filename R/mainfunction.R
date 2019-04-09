#' REDRES
#' @description a package that creates diagnostic plots for mixed models.
#' We we have four types methods to caculate residuals which are pearson residuals, generalized residuals,
#' standardized residuals and conditional residuals. After returning residuals values,
#' the diagnostic plots will be visualized by ggplot.
#'
#' @param model the model we fit from lmer function in lme4 package.
#' @param type  type of residuals. we have generalized, standardized, conditional and pearson residuals.
#'
#' @export
#'
#' @return return residuals according to different types.
#'
#' @examples
#' #get data and fit model
#' library(lme4)
#' x=rep(seq(0,150,by=50),12)
#' f=factor((x+50)/50)
#' g=factor(rep(rep(1:3,each=4),4))
#' block=factor(rep(1:4,each=12))
#' set.seed(532)
#' u=c(rnorm(4,0,6),rnorm(12,0,7))
#' e=rnorm(48,0,6)
#' beta=c(125,15,-10,.4,-0.0015,0,.2)
#' Z1=model.matrix(~0+block)
#' Z2=model.matrix(~0+g:block)
#' Z=cbind(Z1,Z2)
#' y=X%*%beta+Z%*%u+e
#' y=round(y,1)
#' model=lmer(y~g*f+(1|block)+(1|block:g))
#' #use function to get residuals
#' result <- redres(model, type="pearson")
#' # plot diagnostic plots for mixed models
#' library(ggplot2)
#' ResidPlot(model, result)

redres <- function(model, type="raw_cond"){

  assertthat::assert_that(is.character(type), msg='type must be a string.')
  stopifnot(class(model)[1] == "lmerMod")

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
  else if(type=="std_mard"){
    result <- stdres(model, cond = FALSE)
  }
  else if(type=="genres"){
    result <- genres(model)
  }
  else{
  printf("This type is not included in our method, please input a correct one.")
  }

  # Return the residuals
  return(result)

}

# usethis::use_testthat()
# usethis::use_test("redres")
