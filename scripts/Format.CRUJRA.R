rm(list = ls())

library(terra)
library(dplyr)
library(glue)
library(purrr)
library(raster)
library(CausalAI)
library(lubridate)

dest_dir <- "/data/gent/vo/000/gvo00074/felicien/R/outputs/CRUJRA/"
clim_vars <- c("pre","tmp","tmin","tmax","dlwrf","spfh","VPD","dswrf")
# clim_vars <- c("dswrf")

raster.grid = terra::rast(raster(extent(-180, 180,
                                 -25, 25),
                          res = 0.5,
                          crs = "+proj=longlat +datum=WGS84"))
land.frac <- rast(rasterFromXYZ(readRDS("./outputs/landFrac.RDS")))
land.frac.rspld <- terra::resample(land.frac,raster.grid)
land.frac.msk <- land.frac.rspld
land.frac.msk[land.frac.msk < 0.25] <- 0
land.frac.msk[land.frac.msk>=0.25] <- 1

years.ref <- 1900:2100

decades <- c(1980,1990,2000,2010,2020,2023)
# decades <- 2020

message("Reading decade RDS files...")
all_df <- map_dfr(decades, function(decade) {
  readRDS(glue("./outputs/monthly.climate.pantropical.{decade}.RDS")) %>%
    ungroup() %>%
    filter(year >= 1980, abs(lat) <= 25)
})

all_df <- all_df %>%
  mutate(
    lon = as.numeric(lon),
    lat = as.numeric(lat),
    ym  = sprintf("%04d_%02d", year, month)
  ) %>%
  arrange(year, month, lat, lon)


ym_list <- all_df %>% distinct(year, month) %>%
  arrange(year, month) %>%
  transmute(ym = sprintf("%04d_%02d", year, month)) %>%
  pull(ym)

lon_vals <- sort(unique(all_df$lon))
lat_vals <- sort(unique(all_df$lat))

if (length(lon_vals) < 2 || length(lat_vals) < 2) {
  stop("Not enough unique lon/lat values to build a grid.")
}
ext_ <- ext(min(lon_vals), max(lon_vals), min(lat_vals), max(lat_vals))
dx   <- min(diff(lon_vals))
dy   <- min(diff(lat_vals))
template <- rast(ext_, resolution = c(dx, dy), crs = "EPSG:4326")

message(glue("Template grid: {nrow(template)} rows x {ncol(template)} cols @ res {dx} x {dy}"))

# Small helper to rasterize one variable for one year-month
rasterize_one <- function(df_month, var_name, template) {
  vsub <- vect(df_month[, c("lon","lat",var_name)], geom = c("lon","lat"), crs = "EPSG:4326")
  r    <- rasterize(vsub, template, field = var_name)
  r
}

for (v in clim_vars) {
  message(glue("Processing variable: {v}"))

  # Collect layers by month without keeping the whole big data.frame in memory repeatedly
  r_layers <- vector("list", length(ym_list))

  for (i in seq_along(ym_list)) {
    ymk <- ym_list[i]
    # subset data for this month
    dsub <- all_df %>%
      dplyr::filter(ym == ymk) %>%
      dplyr::select(lon, lat, !!sym(v))

    r <- rasterize_one(dsub, v, template)
    names(r) <- ymk
    r_layers[[i]] <- r

    if (i %% 24 == 0) message(glue("  {v}: assembled {i}/{length(ym_list)} monthly layers..."))
  }

  # Combine month layers into one SpatRaster (bands ordered chronologically)
  rstack <- do.call(c, r_layers)
  r.rspld <- terra::resample(rstack,rast(raster.grid))
  r <- terra::mask(project(r.rspld,crs(raster.grid)),
                   land.frac.msk)


  dates <- as.Date(paste0(sapply(strsplit(names(r),"\\_"),"[[",1),"/",
                   sapply(strsplit(names(r),"\\_"),"[[",2),"/01"))

  years <- year(dates)

  t_num <- lubridate::decimal_date(dates)
  r_detr <- terra::app(r, fun = function(v) detrend_vec(v, t_num))
  names(r_detr) <- names(r)

  yrs <- format(dates, "%Y")
  mons_all <- month(dates)
  ref.pos <- yrs %in% years.ref
  ref <- r_detr[[ref.pos]]

  mons <- mons_all[ref.pos]
  clim <- tapp(ref, mons, mean, na.rm = TRUE)

  clim_all <- clim[[mons_all]]
  anoms <- r_detr - clim_all

  writeRaster(r,
              file.path(dest_dir, paste0("abs.",v,".tif")),
              overwrite = TRUE,
              wopt = list(gdal = "COMPRESS=LZW", datatype = "FLT4S"))

  writeRaster(r - anoms,
              file.path(dest_dir, paste0("trends.",v,".tif")),
              overwrite = TRUE,
              wopt = list(gdal = "COMPRESS=LZW", datatype = "FLT4S"))

  writeRaster(r_detr,
              file.path(dest_dir, paste0("detrended.",v,".tif")),
              overwrite = TRUE,
              wopt = list(gdal = "COMPRESS=LZW", datatype = "FLT4S"))

  writeRaster(anoms,
              file.path(dest_dir, paste0("anomaly.",v,".tif")),
              overwrite = TRUE,
              wopt = list(gdal = "COMPRESS=LZW", datatype = "FLT4S"))

}


# scp /home/femeunier/Documents/projects/CausalAI/scripts/Format.CRUJRA.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
