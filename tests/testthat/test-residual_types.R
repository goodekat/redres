context("test-residual_types")

# read data from stat510 of Iowa State University
d = read.delim("http://dnett.github.io/S510/SeedlingDryWeight2.txt")
write.csv(d, "../../../seedlingdrayweight.csv", row.names = FALSE)
d$Genotype = factor(d$Genotype)
d$Tray = factor(d$Tray)
d$Seedling = factor(d$Seedling)
sas_resids <- read.csv(system.file("extdata", "residuals.csv", package = "redres"))
sas_resids$Genotype = factor(sas_resids$Genotype)
sas_resids$Tray = factor(sas_resids$Tray)
sas_resids$Seedling = factor(sas_resids$Seedling)

# fit a mixed model
model <- lmer(SeedlingWeight ~ Genotype + (1|Tray), data = d)

d <- dplyr::full_join(d, sas_resids, by = c("Genotype", "Tray", "Seedling", "SeedlingWeight"))

test_that("residual types work", {

  expect_equal(redres(model), d$ResidCond, tolerance = .00001)
  expect_equal(redres(model, type = "raw_cond"), d$ResidCond, tolerance = .00001)
  expect_equal(redres(model, type = "raw_mar"), d$ResidMar, tolerance = .00001)
  expect_equal(redres(model, type = "pearson_cond"), d$PearsonResidCond, tolerance = .00001)
  expect_equal(redres(model, type = "pearson_mar"), d$PearsonResidMar, tolerance = .00001)

  redres(model, type = "genres")

})

