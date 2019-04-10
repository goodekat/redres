context("test-residual-plots")

model <- lmer(SeedlingWeight ~ Genotype + (1|Tray), data = d)

test_that("plot works", {
  expect_error(ResidPlot(model, type = "response"))
  expect_error(ResidPlot(model, type = raw_mar))
})
