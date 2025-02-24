# Freddie Mac House Price Index
# Downloaded 6/23/2024 from: https://www.freddiemac.com/research/indices/house-price-index
library(tidyverse)
library(broom)
library(here)

regions <- read_csv(here("datasets", "US_regions.csv"))

fmhpi <- read_csv(here("datasets", "fmhpi_master_file.csv")) |>
  filter(GEO_Type == "State") |> # states only
  rename(
    year = Year, month = Month,
    state_abr = GEO_Name, hpi = Index_NSA
  ) |>
  mutate(
    date_dec = year + (month - 1)/12
  ) |>
  left_join(regions, by = "state_abr") |>
  select(-GEO_Type, -GEO_Code, -Index_SA)

write_csv(fmhpi, here("datasets", "fmhpi.csv"))

## example analysis: detrending

fmhpi |>
  filter(state == "Nevada") |>
  ggplot(aes(date_dec, hpi)) + 
  geom_line() +
  scale_y_log10()

# exclude values before 1980 as they follow a totally different trend
fmhpi_trends <- fmhpi |>
  filter(year >= 1980) |>
  nest(data = -state) |>
  mutate(
    # linear trend
    fit_lin = map(data, ~lm(hpi ~ date_dec, data = .x)),
    hpi_trend_lin = map2(fit_lin, data, ~predict(.x, .y)),
    # log trend
    fit_log = map(data, ~lm(log(hpi) ~ date_dec, data = .x)),
    hpi_trend_log = map2(fit_log, data, ~exp(predict(.x, .y)))
  ) |>
  select(-fit_lin, -fit_log) |>
  unnest(cols = c(data, hpi_trend_lin, hpi_trend_log))

fmhpi_trends |>
  filter(state == "California") |>
  ggplot(aes(date_dec, hpi)) + 
  geom_line() +
  geom_line(aes(y = hpi_trend_log), color = "firebrick") +
  scale_y_log10()

fmhpi_trends |>
  filter(state == "California") |>
  ggplot(aes(date_dec, hpi/hpi_trend_log)) + 
  geom_line() +
  scale_y_log10()

fmhpi_trends |>
  filter(state == "California") |>
  ggplot(aes(date_dec, hpi-hpi_trend_lin)) + 
  geom_line()
