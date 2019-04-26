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
#mix <- lmer(height ~ rep + treatment*variety + (1|rep:treatment), data = paprika)
#plot_redres(mix)
#
#mixl <- lmer(log(height) ~ rep + treatment*variety + (1|rep:treatment), data = paprika)
#plot_redres(mixl)
