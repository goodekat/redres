#' Normal quantile plots for random effects
#'
#' @description
#' Plots the vector of all random effects against the normal quantiles. For linear mixed models
#' random effects are assumed to be normally distributed. This plot can be used to assess the
#' validity of that assumption.
#'
#' @param model Model fit using \code{lmer}.
#'
#' @importFrom ggplot2 aes_string ggplot theme_bw xlab ylab
#' @importFrom qqplotr stat_qq_band stat_qq_line stat_qq_point
#' @export plot_raneff
#'
#' @return A normal quantile plot for the random effects.
#'
#' @examples
#' # fits a linear mixed effects model
#' library(lme4)
#' fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#'
#' # plots normal quantiles of random effects vector to assess normality
#' plot_raneff(fm1)

plot_raneff <- function(model){

  # Return an error if an acceptable model type is not entered in the function
  checkmate::expect_class(model, "lmerMod",
                          info = "The input model is not accepted by plot_raneff. Model must be fit using 'lmer'.")

  # building dataframe
  u_df <- data.frame(rand_eff = model@u)

  # quantile plot from qqplotr
  ggplot(data = u_df, aes_string(sample = "rand_eff")) +
    qqplotr::stat_qq_band(bandType = "pointwise", distribution = "norm",
                          fill = "#FBB4AE", alpha = 0.4) +
    qqplotr::stat_qq_line(distribution = "norm", colour = "#FBB4AE") +
    qqplotr::stat_qq_point(distribution = "norm") +
    xlab("Normal quantiles") +
    ylab("Estimated random effects") +
    theme_bw()

}
