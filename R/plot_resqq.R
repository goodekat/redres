#' Generalized residual quantile plot
#'
#' @description
#' Use to compare the generalized residuals to the continuous uniform quantiles.
#' Obvious departures indicate that the assumed model is inadequate to describe the observed data.
#'
#' @param model Model fit using \code{lmer}.
#' @param bandType String identifying confidence band type. The options are "pointwise" (default) and "ts".
#'
#' @importFrom checkmate expect_class
#' @importFrom ggplot2 aes_string ggplot theme_bw xlab ylab
#' @importFrom qqplotr stat_qq_band stat_qq_line stat_qq_point
#' @export plot_genres
#'
#' @return A generalized residual quantile plot.
#'
#' @details
#' Use "pointwise" for bands based on normal confidence intervals. For tail-sensitive confidence
#' bands, use "ts". Tail-sensitive bands are constructed as described by Aldor-Noiman et al. (2013)
#'
#' @references Aldor-Noiman, S. et al. (2013). The Power to See: A New Graphical Test of Normality. The American Statistician. 67:4.
#'
#' @export plot_resqq
#' @examples
#' # fits a linear mixed effect model using lme4 where model has a
#' # random intercept for Days and random slope for Subject*Days
#' library(lme4)
#' fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#'
#' # check that error term is normally distributed
#' plot_resqq(fm1)
#' # quantile plot with tail sensitity confidence bands
#' plot_resqq(fm1, band = "ts")

plot_resqq <- function(model, band = "pointwise"){

  # Return an error if an acceptable model type is not entered in the function
  checkmate::expect_class(model, "lmerMod",
                          info = "The input model is not accepted by plot_resqq. Model must be fit using 'lmer'.")


  checkmate::expect_string(band, info = "The input confidence band type for plot_resqq must be a string.")

  # Return an error if non-supported band type is entered
  if(!(band %in% list("pointwise", "ts"))){
    warning("The confidence band option for quantile plot was not specified correctly.
            The default option will be used. Accepted options are pointwise or ts.")
    band <- "pointwise"
  }
  # checkmate::expect_subset(band, c("pointwise","ts"))

  # building dataframe
  g_df <- data.frame(y = model@resp$y, Residual = redres(model, type = "raw_cond"))

  # quantile plot from qqplotr
  ggplot(data = g_df, aes_string(sample = "Residual")) +
    qqplotr::stat_qq_band(bandType = band, distribution = "norm",
                          fill = "#FBB4AE", alpha = 0.4) +
    qqplotr::stat_qq_line(distribution = "norm", colour = "#FBB4AE") +
    qqplotr::stat_qq_point(distribution = "norm") +
    xlab("Normal quantiles") +
    ylab("Raw conditional residuals") +
    theme_bw()

}
