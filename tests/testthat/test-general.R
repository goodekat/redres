context("test-general")

# test data sets

# read data from stat510 of Iowa State University and adjust factor variables
d <- read.delim("http://dnett.github.io/S510/SeedlingDryWeight2.txt")
d$Genotype <- factor(d$Genotype)
d$Tray <- factor(d$Tray)
d$Seedling <- factor(d$Seedling)
model1 <- lme4::lmer(SeedlingWeight ~ Genotype + (1|Tray), data = d)

# Nettleton's split plot example
block <- factor(rep(1:4, each = 12))
geno <- factor(rep(rep(1:3, each = 4), 4))
fert <- rep(seq(0, 150, by = 50), 12)
X <- model.matrix(~geno+fert+I(fert^2)+geno:fert)
beta <- c(125, 15, -10, 0.4, -0.0015, 0, 0.2)
Z1 <- model.matrix(~0+block)
Z2 <- model.matrix(~0+geno:block)
Z <- cbind(Z1, Z2)
set.seed(20190412)
u <- c(rnorm(4, 0, 6), rnorm(12, 0, 7))
e <- rnorm(48, 0, 6)
resp <- X%*%beta + Z%*%u + e
resp <- round(resp,1)
spexp <- data.frame(block, geno, fert, resp)
spexp$fert <- factor(spexp$fert)
model2 <- lme4::lmer(resp ~ geno*fert + (1|block) + (1|block:geno))

# begin testing
test_that("returns vector of length of data", {
  expect_equal(length(d$SeedlingWeight), length(genres(model1)))
  expect_equal(length(spexp$resp), length(genres(model2)))
  expect_equal(length(model1@resp$y), length(genres(model1)))
  expect_true(is.vector(genres(model1)))
})

test_that("residuals are uniform", {

  # Kolmogorov Smirnov Test
  ks1 <- ks.test(genres(model1), y = "punif")

  expect_true(ks1$p.value = 0.05191, tolerance = 0.001)
  expect_true(ks.test(genres(model2), y = "punif")$p.value > 0.1)

})

test_that("generalized residual plot has correct output",{

})
