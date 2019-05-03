context("test-launch_redres")

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
test_that("check-app-errors", {
  expect_error(launch_redres(1))
  expect_error(launch_redres(model_lm))
  expect_error(launch_redres(c(model, model, model)))
  expect_error(launch_redres(c(model, 1)))
  expect_error(launch_redres(c(model_lm, model)))
  expect_error(create_ui(1))
})

test_that("functions-return-correct-classes", {
  expect_is(create_ui(model), "shiny.tag.list")
  expect_is(create_ui(c(model, model)), "shiny.tag.list")
  expect_is(create_server(model), "function")
  expect_is(create_app(model), "shiny.appobj")
})

library(shinytest)

test_that("app-works", {

  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  expect_pass(testApp("apps/launch_redres/", compareImages = FALSE))
})
