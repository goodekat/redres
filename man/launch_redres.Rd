% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/launch_app.R
\name{launch_redres}
\alias{launch_redres}
\title{Shiny App for 'redressing' linear mixed models}
\usage{
launch_redres(model)
}
\arguments{
\item{model}{A model (or two models wrapped in a list) fit using \code{lmer}.}
}
\value{
A Shiny app with multiple tabs showing diagnostic plots.
}
\description{
This is a shiny app for model diagnostic of mixed model using \code{lmer} function from lme4 package.
This app includes a residual plot computed from six types of residuals (conditional raw, Pearson, and
studentized, marginal raw, Pearson, and studentized) and normal quantile plot using of random effect
and error term. This app can also be used to do model selection through pairwise comparison
two models from different linear mixed models using lmer function.
}
\examples{
\dontrun{
# fits a linear mixed effects model
library(lme4)
fm1 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
launch_redres(model = fm1)
# comparing two different linear mixed effects models
fm1 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
fm2 <- lmer(Reaction ~ Days + (1|Subject) + (0+Days|Subject), sleepstudy)
cmbd <- c(fm1,fm2)
launch_redres(model = cmbd)
}
}
