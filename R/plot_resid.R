#' Diagnostic Residual Plot
#' @description
#' Plots specified residual type by fitted values for a linear mixed effects model
#' fitted using \code{lmer}. Plot used to assess whether the assumptions of constant
#' variance and linear form are adequate.
#'
#' @param model Model fit using \code{lmer}.
#' @param type String identifying type of residual. Default is "raw_cond". See \code{\link{redres}} for details of available types.
#'
#' @export
#'
#' @importFrom broom augment
#' @importFrom ggplot2 aes_string geom_point ggplot xlab ylab geom_hline theme_bw
#'
#' @return A residual plot.
#'
#' @examples
#' # Fit a linear mixed effect model with a default (raw conditional) residual type.
#' library(lme4)
#' fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' # Plot raw conditional residuals by fitted values.
#' plot_redres(fm1)
#' # Plot standardized conditional residuals by fitted values.
#' plot_redres(fm1, type = "std_cond")

plot_redres <- function(model, type = "raw_cond") {

  # Stop if not an lmer model
  if(!(class(model)[1]=="lmerMod")){
    stop("The input model type is not accepted by plot_redres. Model must be fit using 'lmer'.")
  }

  # Stop if residual type is not specified correctly
  type <- tolower(type)
  if(!(type %in% c("raw_cond", "raw_mar", "pearson_cond", "pearson_mar", "std_cond", "std_mar"))){
        stop("Residual type requested is not provided by redres. Please see the documentation for the available types.")
  }

  # Put residuals and fitted values in a data frame
  df <- data.frame(Residual = redres(model = model, type = type),
                   Fitted = broom::augment(model)$.fitted)

  # Specify y-axis label based on residual type
  if (type == "raw_cond") {
    ylabel = "Conditional raw residuals"
  } else if (type == "raw_mar") {
    ylabel = "Marginal raw residuals"
  } else if (type == "pearson_cond") {
    ylabel = "Conditional pearson residuals"
  } else if (type == "pearson_mar") {
    ylabel = "Marginal pearson residuals"
  } else if (type == "std_cond") {
    ylabel = "Conditional studentized residuals"
  } else if (type == "std_mar") {
    ylabel = "Marginal studentized residuals"
  }

  # Create the residual vs fitted plot
  ggplot(df, aes_string(x = "Fitted", y = "Residual")) +
    geom_point() +
    xlab(label = "Fitted values") +
    ylab(label = ylabel) +
    geom_hline(yintercept = 0) +
    theme_bw()

}
