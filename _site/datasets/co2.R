# raw data downloaded from:
# https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_mm_mlo.txt
# see also: https://gml.noaa.gov/ccgg/trends/

library(tidyverse)
library(here)

co2 <- read_table(
  here::here("datasets", "co2_mm_mlo.txt"),
  col_names = FALSE,
  skip = 42
)

names(co2) <- c(
  "year", "month", "date_dec", "co2_ave",
  "co2_deseason", "ndays", "sd_ndays", "unc_mon_mean"
)

co2 <- co2 |>
  mutate(
    ndays = ifelse(ndays < 0, NA, ndays),
    sd_ndays = ifelse(sd_ndays < 0, NA, sd_ndays),
    unc_mon_mean = ifelse(unc_mon_mean < 0, NA, unc_mon_mean)
  )

write_csv(co2, here("datasets", "co2.csv"))

library(cowplot)

# use complete years only
co2_complete <- filter(co2, year >= 1959, year < 2024)

# convert to time series object
co2_ts <- ts(
  data = co2_complete$co2_ave,
  start = 1959,       # data starts Jan 1959
  end = c(2023, 12),  # data ends Dec 2023
  frequency = 12      # we have 12 time points per year
)

# detrend via STL method
# Seasonal Decomposition of Time Series by Loess
# s.window is the span of the loess window; should be odd and at least 7
co2_stl <- stl(co2_ts, s.window = 7)

co2_detrended <- mutate(
  co2_complete,
  seasonal = t(co2_stl$time.series)[1, ],
  trend = t(co2_stl$time.series)[2, ],
  remainder = t(co2_stl$time.series)[3, ]
)
 
# plot
p1 <- ggplot(co2_detrended, aes(date_dec, co2_ave)) + geom_line()
p2 <- ggplot(co2_detrended, aes(date_dec, seasonal)) + geom_line()
p3 <- ggplot(co2_detrended, aes(date_dec, trend)) + geom_line()
p4 <- ggplot(co2_detrended, aes(date_dec, remainder)) + geom_line()
 
plot_grid(p1, p2, p3, p4, ncol = 1, align = 'v')
