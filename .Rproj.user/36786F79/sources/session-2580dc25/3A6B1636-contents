# R/plots_static.R

library(tidyverse)
library(sf)
library(scales)
library(RColorBrewer)

eu_data <- readRDS("data/eu_immigration_panel.rds")

# 1. Time series facetted --------------------------------------------------

plot_immigration_timeseries <- function() {
  eu_data |>
    st_drop_geometry() |>
    filter(!is.na(country)) |>
    ggplot(aes(x = year, y = immigrants, group = country)) +
    geom_line() +
    facet_wrap(~ country, scales = "free_y", ncol = 5) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Immigration in European countries (2010–2019)",
      x = "Year",
      y = "Number of immigrants"
    ) +
    theme_bw()
}


# 2. Choropleth of immigration % (2019) -----------------------------------

plot_immigration_map_pct_2019 <- function() {
  
  eu_2019 <- eu_data |>
    filter(year == 2019)
  
  breaks_pct <- quantile(
    eu_2019$immigration_pct,
    probs = seq(0, 1, length.out = 6),
    na.rm = TRUE
  )
  
  labels_pct <- c(
    paste0("< ", round(breaks_pct[2], 2)),
    paste0(round(breaks_pct[2], 2), "–", round(breaks_pct[3], 2)),
    paste0(round(breaks_pct[3], 2), "–", round(breaks_pct[4], 2)),
    paste0(round(breaks_pct[4], 2), "–", round(breaks_pct[5], 2)),
    paste0("≥ ", round(breaks_pct[5], 2))
  )
  
  eu_2019 |>
    mutate(
      immigration_bin = cut(
        immigration_pct,
        breaks = breaks_pct,
        include.lowest = TRUE,
        labels = labels_pct
      )
    ) |>
    ggplot() +
    geom_sf(aes(fill = immigration_bin), linewidth = 0, alpha = 0.9) +
    theme_void() +
    scale_fill_manual(
      values = brewer.pal(5, "YlOrBr"),
      name = "% immigrants"
    ) +
    labs(
      title = "Immigration as % of population (2019)"
    )
}
