context("test-residual_types")

# read data from stat510 of Iowa State University
d = read.delim("http://dnett.github.io/S510/SeedlingDryWeight2.txt")
d$Genotype = factor(d$Genotype)
d$Tray = factor(d$Tray)
d$Seedling = factor(d$Seedling)

# fit a mixed model
model <- lmer(SeedlingWeight ~ Genotype + (1|Tray), REML = F,data = d)

test_that("residual types work", {

  # redres(model)
  # rc <- redres(model, type = "raw_cond")
  # rm <- redres(model, type = "raw_mar")
  # rpc <- redres(model, type = "pearson_cond")
  # rpm <- redres(model, type = "pearson_mar")
  # gen <- redres(model, type = "genres")
  # data.frame(rc, rm, rpc, rpm, gen)

})
