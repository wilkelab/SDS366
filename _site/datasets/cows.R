# Butterfat percentage for cows
#  
# This dataset corresponds to the `Cows` dataset from the PASWR package. It was
# taken from Sokal and Rohlf.
#
# Source: The Canadian Record of Performance for Purebred Dairy Cattle
library(tidyverse)
library(colorspace)
library(cowplot)

cows <- PASWR::Cows
write_csv(cows, here::here("datasets", "cows.csv"))

# --------------------------------------------------------

cows_filtered <- cows |>
  mutate(breed = as.character(breed)) |>
  filter(breed != "Canadian")

# compute densities for butterfat amount
cows_dens <- group_by(cows_filtered, breed) |>
  do(ggplot2:::compute_density(.$butterfat, NULL)) |>
  rename(butterfat = x)

# get the maximum values
cows_max <- filter(cows_dens, density == max(density)) |>
  ungroup() |>
  mutate(
    hjust = c(0, 0, 0, 0),
    vjust = c(0, 0, 0, 0),
    nudge_x = c(-0.2, -0.2, 0.1, 0.23),
    nudge_y = c(0.03, 0.03, -0.2, -0.06) 
  )

ggplot(cows_dens, aes(x = butterfat, y = density, color = breed, fill = breed)) + 
  geom_density(stat = "identity") +
  geom_text(
    data = cows_max,
    aes(
      label = breed, hjust = hjust, vjust = vjust,
      color = breed,
      x = butterfat + nudge_x, 
      y = density + nudge_y
    ),
    inherit.aes = FALSE,
    size = 12,
    size.unit = "pt"
  ) +
  scale_color_manual(
    values = darken(c("#56B4E9", "#E69F00", "#D55E00", "#009E73"), 0.3),
    breaks = c("Ayrshire", "Guernsey", "Holstein-Friesian", "Jersey"),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c("#56B4E950", "#E69F0050", "#D55E0050", "#009E7350"),
    breaks = c("Ayrshire", "Guernsey", "Holstein-Friesian", "Jersey"),
    guide = "none"
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    labels = scales::percent_format(accuracy = 1, scale = 1),
    name = "butterfat contents"
  ) +
  scale_y_continuous(limits = c(0, 1.99), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_minimal_hgrid() +
  theme(axis.line.x = element_blank())
