# data/01_download_eurostat_data.R

library(tidyverse)
library(eurostat)
library(janitor)
library(sf)

# 1. Country labels --------------------------------------------------------

countries <- c(
  AT = "Austria",     BE = "Belgium",      BG = "Bulgaria",
  CH = "Switzerland", CY = "Cyprus",       CZ = "Czechia",
  DE = "Germany",     DK = "Denmark",      EE = "Estonia",
  EL = "Greece",      ES = "Spain",        FI = "Finland",
  FR = "France",      HR = "Croatia",      HU = "Hungary",
  IE = "Ireland",     IS = "Iceland",      IT = "Italy",
  LI = "Liechtenstein", LT = "Lithuania",  LU = "Luxembourg",
  LV = "Latvia",      MD = "Moldova",      ME = "Montenegro",
  MK = "North Macedonia", MT = "Malta",    NL = "Netherlands",
  NO = "Norway",      PL = "Poland",       PT = "Portugal",
  RO = "Romania",     SE = "Sweden",       SI = "Slovenia",
  SK = "Slovakia",    UK = "United Kingdom"
)


# 2. Immigration data ------------------------------------------------------

immigration_raw <- get_eurostat("migr_imm8") |>
  clean_names()

immigration <- immigration_raw |>
  filter(
    age == "TOTAL",
    sex == "T",
    agedef == "COMPLET",
    geo %in% names(countries)
  ) |>
  rename(
    id = geo,
    immigrants = values
  ) |>
  mutate(
    year = as.integer(substr(time_period, 1, 4)),
    country = countries[id]
  ) |>
  select(id, country, year, immigrants)


# 3. Population data -------------------------------------------------------

population_raw <- get_eurostat("demo_pjan") |>
  clean_names()

population <- population_raw |>
  filter(
    age == "TOTAL",
    sex == "T",
    unit == "NR",
    geo %in% names(countries)
  ) |>
  rename(
    id = geo,
    population = values
  ) |>
  mutate(
    year = as.integer(substr(time_period, 1, 4))
  ) |>
  select(id, year, population)


# 4. Geometry (NUTS0) ------------------------------------------------------

eu_geo <- read_sf(
  "https://raw.githubusercontent.com/eurostat/Nuts2json/master/pub/v2/2021/3035/20M/nutsrg_0.json"
)

st_crs(eu_geo) <- 3035

eu_geo <- eu_geo |>
  filter(id %in% names(countries)) |>
  select(id, geometry)


# 5. Combine into panel ----------------------------------------------------

eu_stats <- population |>
  full_join(immigration, by = c("id", "year"))

eu_data <- eu_geo |>
  left_join(eu_stats, by = "id") |>
  mutate(
    immigration_pc = immigrants / population,
    immigration_pct = 100 * immigration_pc,
    country = if_else(is.na(country), countries[id], country)
  )

# 6. Save outputs ----------------------------------------------------------

if (!dir.exists("data")) dir.create("data")

saveRDS(eu_data, file = "data/eu_immigration_panel.rds")
saveRDS(countries, file = "data/country_labels.rds")
