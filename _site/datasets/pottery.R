library(archdata)
library(here)
data('RBPottery')

write_csv(RBPottery, here("datasets", "pottery.csv"))


library(tidyverse)
library(broom)

pca_fit <- RBPottery |> 
  select(where(is.numeric)) |> # retain only numeric columns
  scale() |>                   # scale to zero mean and unit variance
  prcomp() 

pca_fit |>
  # add PCs to the original dataset
  augment(RBPottery) |>
  ggplot(aes(.fittedPC1, .fittedPC2)) +
  geom_point(aes(color = Region))

arrow_style <- arrow(
  angle = 20, length = grid::unit(8, "pt"),
  ends = "first", type = "closed"
)

pca_fit |>
  # extract rotation matrix
  tidy(matrix = "rotation") |>
  pivot_wider(
    names_from = "PC", values_from = "value",
    names_prefix = "PC"
  ) |>
  mutate(
    # order of variables is:
    # Al2O3, Fe2O3, MgO, CaO, Na2O, K2O, TiO2, MnO, BaO  
    hjust = c(1.05, -.05, -.05, 0.5, 0.5, -.05, 1.05, -.05, 1.05),
    vjust = c(0.5, 0.5, 0.5, -.1, -.1, 0.8, 0.5, 0.2, 0)
  ) |>
  ggplot(aes(PC1, PC2)) +
  geom_segment(
    xend = 0, yend = 0,
    arrow = arrow_style
  ) +
  geom_text(aes(label = column, hjust = hjust, vjust = vjust)) +
  coord_fixed(
    xlim = c(-.7, 0.7),
    ylim = c(-.5, .7) 
  )
