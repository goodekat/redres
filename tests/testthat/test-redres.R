context("test-redres")

# read data from stat510 of Iowa State University and adjust factor variables
d = read.delim("http://dnett.github.io/S510/SeedlingDryWeight2.txt")
d$Genotype = factor(d$Genotype)
d$Tray = factor(d$Tray)
d$Seedling = factor(d$Seedling)

# fit a mixed model
model <- lme4::lmer(SeedlingWeight ~ Genotype + (1|Tray), data = d)

# fit a linear model
model_lm <- lm(SeedlingWeight ~ Genotype, data = d)

# test error messages in redres
test_that("check-redres", {
  expect_error(redres(1))
  expect_error(redres(model_lm))
  expect_error(redres(model, type = "response"))
  expect_error(redres(model, type = raw_mar))
})

# basic checks
test_that("returns vector of length of data", {
  expect_equal(length(d$SeedlingWeight), length(redres(model)))
  expect_type(redres(model), type = 'double')
  expect_true(is.vector(redres(model)))
})

# compare to SAS residuals
test_that("equal-to-SAS", {

  # Load in residuals that were computed using SAS proc mixed for comparison
  sas_resids = read.csv(system.file("extdata", "residuals.csv", package = "redres"))
  sas_resids$Genotype = factor(sas_resids$Genotype)
  sas_resids$Tray = factor(sas_resids$Tray)
  sas_resids$Seedling = factor(sas_resids$Seedling)

  # Join the data and SAS residuals
  joined <- dplyr::full_join(d, sas_resids, by = c("Genotype", "Tray", "Seedling", "SeedlingWeight"))

  # Compare redres and SAS residuals
  expect_equal(redres(model), joined$ResidCond, tolerance = .00001)
  expect_equal(redres(model, type = "raw_cond"), joined$ResidCond, tolerance = .00001)
  expect_equal(redres(model, type = "raw_mar"), joined$ResidMar, tolerance = .00001)
  expect_equal(redres(model, type = "pearson_cond"), joined$PearsonResidCond, tolerance = .00001)
  expect_equal(redres(model, type = "pearson_mar"), joined$PearsonResidMar, tolerance = .00001)
  expect_equal(redres(model, type = "std_cond"), joined$StudentResidCond, tolerance = .00001)
  expect_equal(redres(model, type = "std_mar"), joined$StudentResidMar, tolerance = .00001)

})

