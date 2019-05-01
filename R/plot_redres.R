#' Diagnostic residual plot for linear mixed models
#'
#' @description
#' Creates a plot of residuals versus fitted values or model variable. This plot can be used to assess whether the assumptions of constant
#' variance and linear form assumptions are adequate.
#'
#' @param model Model fit using \code{lmer} from \code{lme4}.
#' @param type String identifying type of residual. Default is "raw_cond".
#'             See \code{\link{redres}} for details of available types.
#' @param xvar String indicates the variable to be plotted at the x-axis. By default,
#'             the fitted values are plotted on the x-axis. Any variable used in the
#'             lmer model can be specified.
#'
#' @importFrom checkmate expect_choice expect_class expect_string
#' @importFrom broom augment
#' @importFrom ggplot2 aes_string geom_hline geom_point ggplot theme_bw xlab ylab
#' @export plot_redres
#'
#' @return A residual plot in the form of a \code{ggplot2} object.
#'
#' @examples
#' # fits a linear mixed effects model
#' library(lme4)
#' fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#'
#' # plots raw conditional residuals by fitted values.
#' plot_redres(fm1)
#'
#' # plots raw conditional residuals by selected variables `Days`.
#' plot_redres(fm1, xvar = "Days")
#'
#' # plots standardized conditional residuals by fitted values.
#' plot_redres(fm1, type = "std_cond")
#'
#' # edits theme of ggplot2 object
#' library(ggplot2)
#' plot_redres(fm1, type = "pearson_mar") + ggtitle("Pearson Marginal Residuals by Fitted Values")

plot_redres <- function(model, type = "raw_cond", xvar = NULL) {

  # Error checks
  checkmate::expect_class(model, "lmerMod",
                          info = "The input model is not accepted by plot_redres. Model must be fit using 'lmer'.")
  checkmate::expect_string(type, info = "The input residual type for plot_redres must be a string.")
  checkmate::expect_choice(type, choices = c("raw_cond", "raw_mar", "pearson_cond", "pearson_mar", "std_cond", "std_mar"),
                           info = "The residual type specified is not available in plot_redres.")
  if(!is.null(xvar) == TRUE) {
    checkmate::expect_string(xvar, info = "The input variable for plot_redres must be a string.")
  }

  # Put residuals and fitted values in a data frame
  df <- data.frame(Residual = redres(model = model, type = type),
                   Fitted = broom::augment(model)$.fitted)
  if(!is.null(xvar) == TRUE) {
    df$Xvar <- broom::augment(model)[[xvar]]
  }

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
  if(!is.null(xvar) == TRUE) {
    ggplot(df, aes_string(x = "Xvar", y = "Residual")) +
      geom_point() +
      xlab(label = xvar) +
      ylab(label = ylabel) +
      geom_hline(yintercept = 0) +
      theme_bw()
  } else {
    ggplot(df, aes_string(x = "Fitted", y = "Residual")) +
      geom_point() +
      xlab(label = "Fitted values") +
      ylab(label = ylabel) +
      geom_hline(yintercept = 0) +
      theme_bw()
  }

}
