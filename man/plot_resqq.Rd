% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_resqq.R
\name{plot_resqq}
\alias{plot_resqq}
\title{Normal quantile plot for residuals}
\usage{
plot_resqq(model)
}
\arguments{
\item{model}{Model fit using \code{lmer} from \code{lme4}.}
}
\value{
A normal quantile plot in the form of a \code{ggplot2} object.
}
\description{
Creates a normal quantile plot of the raw conditional residuals. For linear mixed models,
these residuals are expected to be normally distributed. This plot can be used to assess
this assumption.
}
\details{
Confidence bands are constructed pointwise 95\% normal confidence intervals.
}
\examples{
# fits a linear mixed effect model using lme4 where model has a
# random intercept for Days and random slope for Subject*Days
library(lme4)
fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

# checks that error term is normally distributed
plot_resqq(fm1)

# edits theme of ggplot2 object
library(ggplot2)
plot_resqq(fm1) + theme_grey()
}
