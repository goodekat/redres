# Code for cleaning the paprika data

# Load libraries
library(dplyr)
library(readxl)

# Read in raw data
paprika_raw <- read_excel("data-raw/Height+growth2008.xlsx",
                          sheet = "Original data",
                          range = "A3:J15822")

# Clean data
paprika <- paprika_raw %>%
  filter(Week == 4) %>%
  select(-Year, -Week, -`# leaves`, -`# branches`, -`# Fruits`) %>%
  rename(rep = Rep, treatment = Treatmnt, variety = Variety, plant = `Plant #`, height = Height) %>%
  mutate(rep = factor(rep),
         treatment = factor(treatment),
         variety = factor(variety),
         plant = factor(plant))

# Add the puerto_rico to the data folder
#usethis::use_data(paprika, overwrite = TRUE)

# example models
#mix <- lmer(height ~ rep + treatment*variety + (1|rep:treatment) + (1|rep:treatment:variety), data = paprika)
#plot_redres(mix)
#
#mixl <- lmer(log(height) ~ rep + treatment*variety + (1|rep:treatment) + (1|rep:treatment:variety), data = paprika)
#plot_redres(mixl)
