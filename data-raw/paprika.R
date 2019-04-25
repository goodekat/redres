# Code for cleaning the paprika data

# Load libraries
library(dplyr)
library(readxl)

# Read in raw data
paprika_raw <- read_excel("../Height+growth2008.xlsx",
                          sheet = "Original data",
                          range = "A3:J15822")

# Clean data
paprika <- paprika_raw %>%
  filter(Week == 4) %>%
  select(-Year, -Week, -`Plant #`, -`# leaves`, -`# branches`, -`# Fruits`) %>%
  rename(rep = Rep, treatment = Treatmnt, variety = Variety, height = Height)

# Add the puerto_rico to the data folder
#usethis::use_data(paprika, overwrite = TRUE)

# example models
# mix <- lmer(Height ~ Rep + Treatmnt*Variety + (1|Rep:Treatmnt), data = paprika_sub)
# plot_redres(mix)
#
# mixl <- lmer(log(Height) ~ Rep + Treatmnt*Variety + (1|Rep:Treatmnt), data = paprika_sub)
# plot_redres(mixl)
#
# mixs <- lmer(sqrt(Height) ~ Rep + Treatmnt*Variety + (1|Rep:Treatmnt), data = paprika_sub)
# plot_redres(mixs)
#
# plot_raneff(mix)
