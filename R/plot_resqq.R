#' Generalized residual quantile plot
#'
#' @description
#' Use to compare the generalized residuals to the continuous uniform quantiles.
#' Obvious departures indicate that the assumed model is inadequate to describe the observed data.
#'
#' @param model Model fit using \code{lmer}.
#'
#' @importFrom checkmate expect_class
#' @importFrom ggplot2 aes_string ggplot theme_bw xlab ylab
#' @importFrom qqplotr stat_qq_band stat_qq_line stat_qq_point
#' @export plot_resqq
#'
#' @return A generalized residual quantile plot.
#'
#' @details
#' Confidence bands are constructed from pointwise normal confidence intervals.
#'
#' @examples
#' # fits a linear mixed effect model using lme4 where model has a
#' # random intercept for Days and random slope for Subject*Days
#' library(lme4)
#' fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#'
#' # check that error term is normally distributed
#' plot_resqq(fm1)

plot_resqq <- function(model){

  # Return an error if an acceptable model type is not entered in the function
  checkmate::expect_class(model, "lmerMod",
                          info = "The input model is not accepted by plot_resqq. Model must be fit using 'lmer'.")

  # building dataframe
  g_df <- data.frame(y = model@resp$y, Residual = redres(model, type = "raw_cond"))

  # quantile plot from qqplotr
  ggplot(data = g_df, aes_string(sample = "Residual")) +
    qqplotr::stat_qq_band(bandType = "pointwise", distribution = "norm",
                          fill = "#FBB4AE", alpha = 0.4) +
    qqplotr::stat_qq_line(distribution = "norm", colour = "#FBB4AE") +
    qqplotr::stat_qq_point(distribution = "norm") +
    xlab("Normal quantiles") +
    ylab("Raw conditional residuals") +
    theme_bw()

}
