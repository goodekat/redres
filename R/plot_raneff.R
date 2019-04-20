#' Normal quantile plots for random effects
#' @description
#' Use to compare the random effects to normal quantiles. How to use for diagnosing...
#'
#' @param model Model fit using \code{lmer}.
#'
#' @export
#'
#' @importFrom ggplot2 aes_string ggplot theme_bw xlab ylab
#' @importFrom qqplotr stat_qq_band stat_qq_line stat_qq_point
#'
#' @details
#'
#' Why random effects should be normally distributed and diagnosing qq-plot departures.
#'
#' @examples
#' # Fit a linear mixed effect model with a default (raw conditional) residual type.
#' library(lme4)
#' fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' # Plot normal quantile of random effects vector to assess normality
#' plot_raneff(fm1)

plot_raneff <- function(model){

  # Return an error if an acceptable model type is not entered in the function
  if(!(class(model)[1]=="lmerMod")){
    stop("The input model type is not accepted by plot_genres. Model must be fit using 'lmer'.")
  }

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
