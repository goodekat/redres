

library(readxl)
library(dplyr)

paprika_raw <- read_excel("../Height+growth2008.xlsx",
                          sheet = "Original data",
                          range = "A3:J15822")

paprika_sub <- paprika_raw %>%
  filter(Week == 4) #%>%
  # group_by(Rep, Treatmnt, Variety) %>%
  # summarise(mea)

mix <- lmer(Height ~ Rep + Treatmnt*Variety + (1|Rep:Treatmnt), data = paprika_sub)
plot_redres(mix)

mixl <- lmer(log(Height) ~ Rep + Treatmnt*Variety + (1|Rep:Treatmnt), data = paprika_sub)
plot_redres(mixl)

mixs <- lmer(sqrt(Height) ~ Rep + Treatmnt*Variety + (1|Rep:Treatmnt), data = paprika_sub)
plot_redres(mixs)

plot_raneff(mix)
