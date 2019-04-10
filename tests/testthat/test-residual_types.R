context("test-residual_types")

# read data from stat510 of Iowa State University and adjust factor variables
d = read.delim("http://dnett.github.io/S510/SeedlingDryWeight2.txt")
d$Genotype = factor(d$Genotype)
d$Tray = factor(d$Tray)
d$Seedling = factor(d$Seedling)

# fit a mixed model
model <- lmer(SeedlingWeight ~ Genotype + (1|Tray), data = d)

test_that("equal to SAS", {

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

})

