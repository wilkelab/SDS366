library(tidyverse)
library(mgcv)
library(gratia)
library(gganimate)
library(cowplot)
library(here)

blue_jays <- read_csv(here("datasets", "blue_jays.csv"))

blue_jays_male <- blue_jays |>
  filter(sex == "M")

fit <- gam(head_length_mm ~ body_mass_g, data = blue_jays_male, method = "REML")

blue_jays_new <- tibble(
  body_mass_g = seq(from = 59, to = 82, length.out = 10)
) |>
  mutate(.row = row_number()) # needed to join in fitted samples

# fitted_values returns mean and confidence band, as usual
fv <- fitted_values(fit, data = blue_jays_new)

fs <- fitted_samples(fit, data = blue_jays_new, n = 30, seed = 10) |> 
  left_join(blue_jays_new, by = ".row")

# static plot with mean and confidence band
p <- ggplot(blue_jays_male, aes(body_mass_g)) + 
  geom_ribbon(
    data = fv, aes(ymin = .lower_ci, ymax = .upper_ci),
    fill="grey70", color = NA, alpha = 1/2
  ) +
  geom_point(aes(y = head_length_mm), color = "grey60", size = 1.5) +
  #geom_line(data = sample_df, aes(group = .draw), color = "#0072B2", linewidth = 0.3) +
  geom_line(
    data = fv, aes(y = .fitted),
    color = "#0072B2", linewidth = 1
  ) +
  scale_x_continuous(
    limits = c(59, 82),
    expand = c(0, 0),
    name = "body mass (g)") +
  scale_y_continuous(
    limits = c(52, 61),
    expand = c(0, 0),
    name = "head length (mm)"
  ) +
  theme_half_open()
p

ggsave(
  here("assets", "visualizing-uncertainty", "blue-jays-static.png"),
  p,
  width = 5.5, height = (3/4)*5.5,
  dpi = 300,
  bg = "white"
)

a <- ggplot(blue_jays_male, aes(body_mass_g)) + 
  geom_ribbon(
    data = fv, aes(ymin = .lower_ci, ymax = .upper_ci),
    fill="grey70", color = NA, alpha = 1/2
  ) +
  geom_point(aes(y = head_length_mm), color = "grey60", size = 1.5) +
  geom_line(
    data = fv, aes(y = .fitted),
    color = "grey70", linewidth = .6
  ) +
  geom_line(
    data = fs,
    aes(y = .fitted, group = .draw),
    color = "#0072B2", linewidth = 1
  ) +
  scale_x_continuous(
    limits = c(59, 82),
    expand = c(0, 0),
    name = "body mass (g)") +
  scale_y_continuous(
    limits = c(52, 61),
    expand = c(0, 0),
    name = "head length (mm)"
  ) +
  theme_half_open() +
  transition_manual(.draw)

a

animate(
  a,
  fps = 5,
  width = 5.5*100, height = (3/4)*5.5*100,
  res = 100
)

anim_save(
  here("assets", "visualizing-uncertainty", "blue-jays-HOP.gif"),
  a,
  nframes = 60,
  fps = 5,
  width = 5.5*300, height = (3/4)*5.5*300,
  res = 300
)
