# Dependencies ------

if (!require("pacman")) install.packages("pacman")

pacman::p_install_version(
  c("lubridate", "data.table", "dplyr", "forecast"),
  c("1.6.0", "1.10.1", "0.5.0", "7.0")
)

pacman::p_load(data.table, dplyr, dtplyr, lubridate, ggplot2, forecast)

# Import data ------

import <- fread("data/raw/training.csv")

import$purchase_date <- ymd(import$purchase_date)