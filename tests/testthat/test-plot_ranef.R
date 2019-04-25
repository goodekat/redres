context("test-plot_ranef")

# read data from stat510 of Iowa State University and adjust factor variables
d = read.delim("http://dnett.github.io/S510/SeedlingDryWeight2.txt")
d$Genotype = factor(d$Genotype)
d$Tray = factor(d$Tray)
d$Seedling = factor(d$Seedling)

# fit a mixed model
model <- lme4::lmer(SeedlingWeight ~ Genotype + (1|Tray), data = d)

# fit a linear model
model_lm <- lm(SeedlingWeight ~ Genotype, data = d)

# fit model with multiple random terms
data(sleepstudy, package = "lme4")
model_2 <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

# test error messages
test_that("check-plot_raneff", {
  expect_error(plot_ranef(1))
  expect_error(plot_ranef(model_lm))
})

# check for doppelgangers
test_that("validate-plot_ranef", {
  vdiffr::expect_doppelganger("one_effect", plot_ranef(model))
})
