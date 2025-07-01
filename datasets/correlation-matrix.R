library(tidyverse)
library(here)
library(colorspace)
library(cowplot)

# correlation data taken from
# Spellman et al., Comprehensive identification of cell cycle-regulated
# genes of the yeast Saccharomyces cerevisiae by microarray
# hybridization. Mol. Biol. Cell, 1998
# https://pubmed.ncbi.nlm.nih.gov/9843569/
# PMCID: PMC25624
# data is correlation coefficients between different yeast genes
# across cellular states
data <- 
  read_csv("https://tidyplots.org/data/correlation-matrix.csv")

cm <- df |>
  select(x, y, correlation) |>
  pivot_wider(names_from = x, values_from = correlation) |>
  column_to_rownames("y") |>
  as.matrix()

clust <- hclust(as.dist(1-cm), method="average") 
levels <- clust$labels[clust$order]

data_final <- data |>
  mutate(
    x = factor(x),
    y = factor(y),
    x_ordered = factor(x, levels = levels),
    y_ordered = factor(y, levels = levels)
  ) |>
  select(
    x, y, x_ordered, y_ordered, correlation
  )

saveRDS(data_final, here("datasets", "correlation-matrix.rds"))

make_plot <- function(data) {
  data |>
  filter(as.integer(x) < as.integer(y)) |>
    ggplot(aes(x, y, fill = correlation)) + 
    geom_tile() +
    scale_x_discrete(
      name = "gene 1",
      position = "top"
    ) +
    scale_y_discrete(
      name = "gene 2"
    ) +
    scale_fill_continuous_divergingx(
      palette = "PRGn",
      limits = c(-0.85, 1)
    ) +
    coord_fixed() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      legend.title = element_text(hjust = 0.5),
      legend.position = "inside",
      legend.position.inside = c(1, 0),
      legend.justification = c(1, 0)
    )
}

data_final |>
  make_plot()
  
data_final |>
  mutate(
    x = x_ordered,
    y = y_ordered
  ) |>
  make_plot()
