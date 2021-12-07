# calecopal demo script
# December 7, 2021
# Heili Lowman

# The following script will demonstrate some capabilities
# of the calecopal R package.

# To install the 'calecopal' package, run the following:
# library(devtools)
# devtools::install_github("an-bui/calecopal")

# We'll also use the palmerpenguins dataset.
# To install the 'palmerpenguins' package, run the following:
# install_packages("palmerpenguins")

# Load packages
library(tidyverse)
library(calecopal)
library(palmerpenguins)

# Load data (base R dataset)
dat <- penguins

# Create a basic scatterplot.
# Using basic "lake" palette of 5 colors.
fig1 <- ggplot(dat, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = island)) +
  scale_color_manual(values = cal_palette("lake")) +
  labs(x = "Bill Length (mm)",
       y = "Bill Depth (mm)",
       color = "Island") +
  theme_bw()

fig1

# Using "desert" palette with continuous scale.
fig2 <- dat %>%
  mutate(body_mass_f = factor(case_when(body_mass_g <= 3000 ~ "XXS",
                                        body_mass_g > 3000 & body_mass_g <= 3500 ~ "XS",
                                        body_mass_g > 3500 & body_mass_g <= 4000 ~ "XS",
                                        body_mass_g > 4000 & body_mass_g <= 4500 ~ "S",
                                        body_mass_g > 4500 & body_mass_g <= 5000 ~ "M",
                                        body_mass_g > 5000 & body_mass_g <= 5500 ~ "L",
                                        body_mass_g > 5500 & body_mass_g <= 6000 ~ "XL",
                                        body_mass_g > 6000 ~ "XXL"))) %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = body_mass_f)) +
  scale_color_manual(values = cal_palette("desert", n = 8, type = "continuous")) +
  labs(x = "Bill Length (mm)",
       y = "Bill Depth (mm)",
       color = "Body Mass (g)") +
  theme_bw()

fig2

# More palettes and information available at https://github.com/an-bui/calecopal

# End of script.
