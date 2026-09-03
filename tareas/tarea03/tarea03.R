library(tidyverse)
library(palmerpenguins)
penguins
glimpse(penguins)
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point()
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point()
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point() +
  geom_smooth(method = "lm")
ggplot(penguins, aes(x = species)) +
  geom_bar()
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()