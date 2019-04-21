#' Compute residuals for a linear mixed model
#'
#' @description
#' Computes residuals for linear mixed models fit using \code{lmer} from the lme4.
#' It can return seven types of residuals inlcuding options that are not available
#' from lme4. These types are conditional raw, Pearson, and studentized, marginal
#' raw, Pearson, and studentized, and generalized residuals.
#'
#' @usage redres(model, type = "raw_cond")
#'
#' @param model Model fit using \code{lmer} from lme4 for which residuals will be computed.
#' @param type String identifying type of residual. Default is "raw_cond". See details for the options available.
#'
#' @details
#' Residual types available
#' \itemize{
#'   \item \code{"genres"}: generalized residuals
#'   \item \code{"pearson_cond"}: Pearson conditional residuals
#'   \item \code{"pearson_mar"}: Pearson marginal residuals
#'   \item \code{"raw_cond"}: raw conditional residuals (default)
#'   \item \code{"raw_mar"}: raw marginal residuals
#'   \item \code{"std_cond"}: studentized conditional residuals
#'   \item \code{"std_mar"}: studentized marginal residuals
#' }
#' See the vignette for details on how residual types are computed.
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate expect_string
#' @export redres
#'
#' @return Returns residuals according to type specified.
#'
#' @examples
#' # fits a linear mixed effects model
#' library(lme4)
#' fm1 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#'
#' # computes the default residuals (raw conditional)
#' redres(fm1)
#'
#' # changes the residual type to studentized marginal
#' redres(fm1, type = "std_mar")

redres <- function(model, type = "raw_cond"){

  # Error checks
  checkmate::expect_class(model, "lmerMod",
                          info = "The input model is not accepted by redres. Model must be fit using 'lmer'.")
  checkmate::expect_string(type, info = "The input residual type for redres must be a string.")
  checkmate::expect_choice(type, choices = c("raw_cond", "raw_mar", "pearson_cond", "pearson_mar", "std_cond", "std_mar", "genres"),
                           info = "The residual type specified is not available in redres.")


  # Compute residuals based on type specified
  if(type == "raw_cond"){
    result <- rawres(model, cond = TRUE)
  }
  else if(type == "raw_mar"){
    result <- rawres(model, cond = FALSE)
  }
  else if(type == "pearson_cond"){
    result <- pearsonres(model, cond = TRUE)
  }
  else if(type == "pearson_mar"){
    result <- pearsonres(model, cond = FALSE)
  }
  else if(type == "std_cond"){
    result <- stdres(model, cond = TRUE)
  }
  else if(type == "std_mar"){
    result <- stdres(model, cond = FALSE)
  }
  else if(type == "genres"){
    result <- genres(model)
  }

  # Return the residuals
  return(result)

}

