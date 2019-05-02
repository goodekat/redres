#' Compute residuals for a linear mixed model
#'
#' @description
#' Computes residuals for a given linear mixed model.
#'
#' @usage compute_redres(model, type = "raw_cond")
#'
#' @param model Model fit using \code{lmer} from \code{lme4}.
#' @param type String identifying type of residual. Default is "raw_cond". See details for the options available.
#'
#' @details
#' Residual types available:
#' \itemize{
#'   \item \code{"pearson_cond"}: Pearson conditional residuals
#'   \item \code{"pearson_mar"}: Pearson marginal residuals
#'   \item \code{"raw_cond"}: raw conditional residuals (default)
#'   \item \code{"raw_mar"}: raw marginal residuals
#'   \item \code{"std_cond"}: studentized conditional residuals
#'   \item \code{"std_mar"}: studentized marginal residuals
#' }
#' See the \href{https://goodekat.github.io/redres/articles/redres-vignette.html#extracting-residuals}{vignette} for details on how residual types are computed.
#'
#' @importFrom checkmate expect_choice expect_class expect_string
#' @export compute_redres
#'
#' @return Returns a vector of residuals according to type specified. Residuals appear in the same order as the observations used to fit the model.
#'
#' @examples
#' # fits a linear mixed effects model
#' library(lme4)
#' fm1 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#'
#' # computes the default residuals (raw conditional)
#' compute_redres(fm1)
#'
#' # changes the residual type to studentized marginal
#' compute_redres(fm1, type = "std_mar")

compute_redres <- function(model, type = "raw_cond"){

  # Error checks
  checkmate::expect_class(model, "lmerMod",
                          info = "The input model is not accepted by compute_redres. Model must be fit using 'lmer'.")
  checkmate::expect_string(type, info = "The input residual type for compute_redres must be a string.")
  checkmate::expect_choice(type, choices = c("raw_cond", "raw_mar", "pearson_cond", "pearson_mar", "std_cond", "std_mar"),
                           info = "The residual type specified is not available in compute_redres.")


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

  # Return the residuals
  return(result)

}

