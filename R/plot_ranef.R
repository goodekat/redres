#' Normal quantile plots for random effects
#'
#' @description
#' Plots each random effect in the model against the normal quantiles. For linear mixed models
#' random effects are assumed to be normally distributed. This plot can be used to assess the
#' validity of that assumption.
#'
#' @param model Model fit using \code{lmer}.
#'
#' @importFrom cowplot plot_grid
#' @importFrom ggplot2 aes_string ggplot ggplotGrob theme_bw xlab ylab
#' @importFrom gridExtra grid.arrange
#' @importFrom lme4 ranef
#' @importFrom purrr pmap
#' @importFrom qqplotr stat_qq_band stat_qq_line stat_qq_point
#' @importFrom tidyr nest
#' @export plot_ranef
#'
#' @return A grid of normal quantile plots for the random effects.
#'
#' @examples
#' # fits a linear mixed effects model
#' library(lme4)
#' fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#'
#' # plots normal quantiles of random effects vector to assess normality
#' plot_ranef(fm1)

plot_ranef <- function(model){

  # Return an error if an acceptable model type is not entered in the function
  checkmate::expect_class(model, "lmerMod",
                          info = "The input model is not accepted by plot_raneff. Model must be fit using 'lmer'.")

  # building dataframe of all random effects
  bmat <- as.data.frame(lme4::ranef(model))

  # converting each random effect vector into one line with nest
  renest <- tidyr::nest(bmat, -grpvar, -term)

  # generating list of ggplot objects for each random effect vector
  plots <- purrr::pmap(list(renest$data, renest$grpvar, renest$term),
                          function(a,b,c){
                            ggplot(data = a, aes_string(sample = "condval")) +
                              qqplotr::stat_qq_band(bandType = "pointwise",
                                                    distribution = "norm",
                                                    fill = "#FBB4AE", alpha = 0.4) +
                              qqplotr::stat_qq_line(distribution = "norm", colour = "#FBB4AE") +
                              qqplotr::stat_qq_point(distribution = "norm") +
                              xlab("Normal quantiles") + theme_bw() +
                              ylab(paste(b,": ", c))
                          }
  )

  # Turn the list of plots into a grob
  #my_grobs <- lapply(plots, ggplot2::ggplotGrob)

  # Create grid of individual plots specified
  #gridExtra::grid.arrange(grobs = my_grobs, ncol = nrow(renest))
  cowplot::plot_grid(plotlist = plots)
}
