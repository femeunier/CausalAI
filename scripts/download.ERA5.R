library(reticulate)
library(future)
library(purrr)
library(furrr)
library(lubridate)

# setwd("/home/femeunier/Documents/projects/YGB/outputs/") # change this to your own working directory
setwd("/data/gent/vo/000/gvo00074/ED_common_data/met/Tropics/")

#plan(multicore)

years   <- 1980:2024
months  <- sprintf("%02d",12)
hours8  <- c("00:00","03:00","06:00","09:00","12:00","15:00","18:00","21:00")

# To avoid queue errors, keep this sequential or 1–2 workers max
plan(sequential)

# Helper: month -> valid day strings (handles month length & leap years)
days_in_month_str <- function(y, m) sprintf("%02d", 1:days_in_month(ymd(sprintf("%04d-%02d-01", y, as.integer(m)))))

reticulate::use_virtualenv("r-reticulate", required = TRUE)
cdsapi <- import("cdsapi")
c <- cdsapi$Client()


cross2(years, months) %>%
  future_walk(function(ym) {
    y <- ym[[1]]; m <- ym[[2]]
    out <- sprintf("ERA5_SWC_%04d_%s.GRIB", y, m)
    if (file.exists(out)) return(NULL)

    c$retrieve(
      "reanalysis-era5-land",
      dict(
        variable = list(
          "volumetric_soil_water_layer_1",
          "volumetric_soil_water_layer_2",
          "volumetric_soil_water_layer_3",
          "volumetric_soil_water_layer_4"
        ),
        year   = as.character(y),
        month  = m,
        day    = days_in_month_str(y, m),
        time   = hours8,
        area   = "25/-180/-25/180",   # N/W/S/E
        grid   = "0.5/0.5",
        format = "GRIB"
      ),
      out
    )
    NULL
  })


# scp /home/femeunier/Documents/projects/CausalAI/scripts/download.ERA5.R hpc:/data/gent/vo/000/gvo00074/felicien/R



