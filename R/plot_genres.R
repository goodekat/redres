#' Generalized residual quantile plot
#'
#' @description
#' Use to compare the generalized residuals to the uniform quantiles. Obvious departures indicate
#' a lack of fit.
#'
#' @param model Model fit using \code{lmer} from lme4 package.
#'
#' @importFrom checkmate expect_class
#' @importFrom ggplot2 aes_string ggplot theme_bw xlab ylab
#' @importFrom qqplotr stat_qq_band stat_qq_line stat_qq_point
#' @export plot_genres
#'
#' @return A generalized residual quantile plot.
#'
#' @details
#' First section what is a generalized residual. Reiterate diagnostics.
#' Second section assumptions of lmer model
#'
#' @examples
#' # fits a linear mixed effect model using lme4 where model has a
#' # random intercept for Days and random slope for Subject*Days
#' library(lme4)
#' fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#'
#' # check that all model assumptions are reasonable using generalized residuals
#' plot_genres(fm1)

plot_genres <- function(model){

  # Return an error if an acceptable model type is not entered in the function
  checkmate::expect_class(model, "lmerMod",
                          info = "The input model is not accepted by plot_genres. Model must be fit using 'lmer'.")

  # building dataframe
  g_df <- data.frame(y = model@resp$y, Residual = genres(model))

  # quantile plot from qqplotr
  ggplot(data = g_df, aes_string(sample = "Residual")) +
    qqplotr::stat_qq_band(bandType = "pointwise", distribution = "unif",
                          fill = "#FBB4AE", alpha = 0.4) +
    qqplotr::stat_qq_line(distribution = "unif", colour = "#FBB4AE") +
    qqplotr::stat_qq_point(distribution = "unif") +
    xlab("Uniform quantiles") +
    ylab("Generalized residuals") +
    theme_bw()

}
