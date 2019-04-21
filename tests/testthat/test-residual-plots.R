context("test-residual-plots")

# read data from stat510 of Iowa State University and adjust factor variables
d = read.delim("http://dnett.github.io/S510/SeedlingDryWeight2.txt")
d$Genotype = factor(d$Genotype)
d$Tray = factor(d$Tray)
d$Seedling = factor(d$Seedling)

# fit a mixed model
model <- lme4::lmer(SeedlingWeight ~ Genotype + (1|Tray), data = d)

test_that("check-errors", {
  expect_error(ResidPlot(model, type = "response"))
  expect_error(ResidPlot(model, type = raw_mar))
})

test_that("validate-plots", {
  vdiffr::expect_doppelganger("basic", plot_redres(model))
})
