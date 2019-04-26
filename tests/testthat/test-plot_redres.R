context("test-plot_resid")

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
test_that("check-plot_resid", {
  expect_error(plot_resid(1))
  expect_error(plot_resid(model_lm))
  expect_error(plot_resid(model, type = "genres"))
  expect_error(plot_resid(model, type = raw_mar))
  expect_error(plot_resid(model, xvar = Genotype))
  expect_error(plot_resid(model, xvar = Genotype, type = "pearson_mar"))
})

# check for doppelgangers
test_that("validate-plot_resid", {
  vdiffr::expect_doppelganger("basic", plot_redres(model))
  vdiffr::expect_doppelganger("raw_mar", plot_redres(model, type = "raw_mar"))
  vdiffr::expect_doppelganger("pearson_cond", plot_redres(model, type = "pearson_cond"))
  vdiffr::expect_doppelganger("pearson_mar", plot_redres(model, type = "pearson_mar"))
  vdiffr::expect_doppelganger("std_cond", plot_redres(model, type = "std_cond"))
  vdiffr::expect_doppelganger("std_mar", plot_redres(model, type = "std_mar"))
  vdiffr::expect_doppelganger("xvar", plot_redres(model, xvar = "Genotype"))
  vdiffr::expect_doppelganger("xvar_person_mar", plot_redres(model, xvar = "Genotype", type = "pearson_mar"))

})
