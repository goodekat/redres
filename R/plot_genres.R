# plots generalized residuals against uniform quantiles

#' @import ggplot2
#' @import qqplotr
# Notes: not sure what all I should import from ggplot2 and qqplotr
# Also need to pick a color palette

plot_genres <- function(model, bandType = "ts"){

  # building dataframe
  g_df <- data.frame(y = model@resp$y, Residual = genres(model))

  # quantile plot from qqplotr
  ggplot(data = g_df, aes(sample = Residual)) +
    qqplotr::stat_qq_band(bandType = bandType, fill = "#8DA0CB",
                          alpha = 0.4, distribution = "unif") +
    qqplotr::stat_qq_line(colour = "#8DA0CB", distribution = "unif") +
    qqplotr::stat_qq_point(distribution = "unif") +
    xlab("Uniform quantiles") +
    ylab("generalized residuals") +
    theme_bw()
}
