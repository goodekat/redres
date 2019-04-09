#' Residual Plots with multiple residual types choices given a model.
#' Now have "lm", "glm", "lmer" three kinds of models, except the residual
#' types included in those models, we also have other residual types, such as
#' conditional residual, generalized residual and standrized residual.
#'
#' @param model Model fit using either \code{lm}, \code{glm}, \code{lmer},
#'   \code{redres}.
#' @param type Type of residuals to plot. For model \code{lm}, we have
#' "response", "pearson", "standardized"; for \code{glm}, we have "response",
#' "pearson", "deviance"; for \code{lmer}, we have "response", "pearson",
#' "condres", "stdres", "genres", note the last three types are coded by ourself.
#'
#' @export
#' @import ggplot2 dplyr broom lme4
#' @return A residual plot.
#'
#' @examples
#' # Fit a linear regression model
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' lm.D9 <- lm(weight ~ group)
#' ResidPlot(lm.D9, "pearson")
#'
#' # Fit a generalized linear regression model.
#' utils::data(anorexia, package = "MASS")
#' anorex.1 <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
#' family = gaussian, data = anorexia)
#' ResidPlot(anorex.1, "deviance")
#'
#' # Fit a linear mixed effect model with a default residual type.
#' fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' ResidPlot(fm1, "response")
#' # Fit a linear mixed effect model with a conditional residual type.
#' d = read.delim("http://dnett.github.io/S510/SeedlingDryWeight2.txt")
#' d$Genotype = factor(d$Genotype)
#' model <- lmer(SeedlingWeight ~ Genotype + (1|Tray), REML = F,data = d)
#' ResidPlot(model, type = "condres")

ResidPlot <- function(model, type) {
  if(!(class(model)[1] %in% c("lm", "glm", "lmerMod")))
    stop("Models must be one of 'lm', 'glm' or 'lmer'.")

  type <- tolower(type)
  if(!is.na(type)){
    if(class(model)[1] == "lm"){
      if(!(type %in% c("response", "pearson", "standardized"))){
        stop("Residual type for 'lm' model is not correct, please see the function documentation.")
      }
    }
    else if(class(model)[1] == "glm"){
        if(!(type %in% c("response", "pearson", "deviance"))){
          stop("Residual type for 'glm' model is not correct, please see the function documentation.")
        }
    }
    else if(class(model)[1] == "lmerMod"){
      if(!(type %in% c("response", "pearson", "condres", "stdres", "genres"))){
        stop("Residual type for 'lmer' model is not correct, please see the function documentation.")
      }
    }
  }

  if(is.na(type)){
    df <- data.frame(Residual = helper_resid(type = NA, model = model))
  } else{
    df <- data.frame(Residual = helper_resid(type = type, model = model))
  }

 ggplot(df, aes(x = broom::augment(model)$.fitted, y = Residual)) +
    geom_point() + xlab(label = "fitted")
}
