context("test-plot_raneff")

# read data from stat510 of Iowa State University and adjust factor variables
d = read.delim("http://dnett.github.io/S510/SeedlingDryWeight2.txt")
d$Genotype = factor(d$Genotype)
d$Tray = factor(d$Tray)
d$Seedling = factor(d$Seedling)

# fit a mixed model
model <- lme4::lmer(SeedlingWeight ~ Genotype + (1|Tray), data = d)

# fit a linear model
model_lm <- lm(SeedlingWeight ~ Genotype, data = d)

# test error messages
test_that("check-plot_raneff", {
  expect_error(plot_raneff(1))
  expect_error(plot_raneff(model_lm))
})

# check for doppelgangers
test_that("validate-plot_raneff", {
  vdiffr::expect_doppelganger("default", plot_raneff(model))
})
