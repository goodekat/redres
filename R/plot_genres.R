#' Generalized residual quantile plot
#' @description
#' Use to compare the generalized residuals to the uniform quantiles. Obvious departures indicate
#' a lack of fit.
#'
#' @param model
#' @importFrom  ggplot2 aes_string ggplot theme_bw xlab ylab
#' @importFrom qqplotrstat_qq_band stat_qq_line stat_qq_point
# Notes: not sure what all I should import from ggplot2 and qqplotr
# Also need to pick a color palette

plot_genres <- function(model){

  # building dataframe
  g_df <- data.frame(y = model@resp$y, Residual = genres(model))

  # quantile plot from qqplotr
  ggplot(data = g_df, aes_string(sample = "Residual")) +
    qqplotr::stat_qq_band(bandType = "ts", fill = "#8DA0CB",
                          alpha = 0.4, distribution = "unif") +
    qqplotr::stat_qq_line(colour = "#8DA0CB", distribution = "unif") +
    qqplotr::stat_qq_point(distribution = "unif") +
    xlab("Uniform quantiles") +
    ylab("generalized residuals") +
    theme_bw()
}
