library(tidyverse)
library(here)
library(colorspace)
library(cowplot)

# Data for correlation example taken from:
# Spellman et al., Comprehensive identification of cell cycle-regulated
# genes of the yeast Saccharomyces cerevisiae by microarray
# hybridization. Mol. Biol. Cell, 1998
# https://pubmed.ncbi.nlm.nih.gov/9843569/
# PMCID: PMC25624
# Available data is gene express profiles over time for thousands of yeast
# genes. We calculate correlation coefficients between these expression profiles,
# retaining only genes with few missing data values and that are on average
# the most strongly correlated with other genes.

# data downloaded from: https://pmc.ncbi.nlm.nih.gov/articles/instance/25624/bin/mbc_9_12_3273__CDCDATA.TXT
df <- read_tsv(here("datasets", "mbc_9_12_3273__CDCDATA.txt")) |>
  select(-`cln3 experiment1`, -`cln3 experiment2`, -`clb2 experiment2`, -`clb2 experiment1`) |>
  rename(gene = `...1`) |>
  pivot_longer(-gene, names_to = "experiment", values_to = "expr")

# find genes with fewer than 6 missing values
gene_list_low_na <- df |>
  group_by(gene) |>
  summarize(
    na_count = sum(is.na(expr))
  ) |>
  filter(na_count < 6) |>
  pull(gene)

# retain only those genes
df <- filter(df, gene %in% gene_list_low_na)

# calculate correlation matrix
cm <- df |>
  pivot_wider(names_from = "gene", values_from = "expr") |>
  select(-experiment) |>
  cor(use="pairwise.complete.obs")

# find genes with the highest average absolute correlations
gene_list_high_cor <- as.data.frame(cm) |> 
  rownames_to_column("gene") |>
  pivot_longer(-gene, names_to = "gene2", values_to = "cor") |>
  group_by(gene) |>
  summarize(
    meancor = mean(abs(cor))
  ) |>
  arrange(desc(meancor)) |>
  slice(1:600) |>  # retain 600 top genes
  pull(gene)

# recalculate correlation matrix for top 600 genes from
# previous analysis
cm2 <- df |>
  filter(gene %in% gene_list_high_cor) |>
  pivot_wider(names_from = "gene", values_from = "expr") |>
  select(-experiment) |>
  cor(use="pairwise.complete.obs")

clust <- hclust(as.dist(1-cm2), method="average") 
levels <- clust$labels[clust$order]

data_final <- as.data.frame(cm2) |>
  rownames_to_column("x") |>
  pivot_longer(-x, names_to = "y", values_to = "correlation") |>
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


#-------------------------------------------------------
# Plotting example

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

