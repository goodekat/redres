context("test-plot_resqq")

# read data from stat510 of Iowa State University and adjust factor variables
d = read.delim("http://dnett.github.io/S510/SeedlingDryWeight2.txt")
d$Genotype = factor(d$Genotype)
d$Tray = factor(d$Tray)
d$Seedling = factor(d$Seedling)

# fit a mixed model
model <- lme4::lmer(SeedlingWeight ~ Genotype + (1|Tray), data = d)

# fit a linear model
model_lm <- lm(SeedlingWeight ~ Genotype, data = d)

test_that("check-errors", {

  # not a model
  expect_error(plot_resqq(1))

  # not a model fit using lmer
  expect_error(plot_resqq(model_lm))

  # band input not a string
  expect_error(plot_resqq(model, band = ts))

  # incorrect band type
  expect_that(plot_resqq(model, band = "boot"), gives_warning())
})

test_that("validate-plot_bands", {
  vdiffr::expect_doppelganger("pointwise_b", plot_resqq(model))
  vdiffr::expect_doppelganger("ts_b", plot_resqq(model, band = "ts"))
})
