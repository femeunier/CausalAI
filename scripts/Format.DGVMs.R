rm(list = ls())

library(ncdf4)
library(dplyr)
library(lubridate)
library(TrENDY.analyses)
library(reshape2)
library(raster)
library(terra)
library(glue)

models <- rev(c("CABLE-POP","CLASSIC","CLM6.0",
            "E3SM","JSBACH","JULES","LPJ-GUESS",
            "LPJmL","LPX-Bern","VISIT"))

main_dir <- "/data/gent/vo/000/gvo00074/felicien/R/outputs/DGVM"

for (imodel in seq(1,length(models))){

  cmodel <- models[imodel]

  dest_dir <- file.path(main_dir,cmodel)
  dir.create(dest_dir,showWarnings = FALSE)

  # First the SWC

  SWC <- readRDS(paste0("./outputs/MSL.grid.",cmodel,".RDS")) %>%
    mutate(month = month(time),
           year = year(time)) %>%
    dplyr::select(-time)

  vars <- SWC %>%
    dplyr::select(-any_of(c("lon","lat","year","month"))) %>%
    colnames()

  df <- SWC %>%
    ungroup() %>%
    filter(year >= 1980) %>%
    filter(abs(lat) <= 25) %>%
    mutate(ym = sprintf("%04d_%02d", year, month)) %>%
    arrange(year, month)

  ym_list <- unique(df %>%
                      mutate(ym = sprintf("%04d_%02d", year, month)) %>%
                      pull(ym))

  for (ymk in ym_list) {

    dsub <- df %>%
      mutate(ym = sprintf("%04d_%02d", year, month)) %>%
      filter(ym == ymk) %>%
      mutate(
        lon = as.numeric(lon),
        lat = as.numeric(lat))

    # Build a template grid from lon/lat
    lon_vals <- sort(unique(dsub$lon))
    lat_vals <- sort(unique(dsub$lat))
    ext <- ext(min(lon_vals), max(lon_vals), min(lat_vals), max(lat_vals))
    dx <- min(diff(lon_vals))
    dy <- min(diff(lat_vals))
    tem <- rast(ext, resolution = c(dx, dy), crs = "EPSG:4326")

    # Rasterize each variable
    layers <- lapply(vars, function(v) {
      vsub <- vect(dsub[, c("lon","lat",v)],
                   geom = c("lon","lat"), crs = "EPSG:4326")
      r <- rasterize(vsub, tem, field = v)
      names(r) <- v
      r
    })

    # Combine all variables into one multilayer raster
    rstack <- do.call(c, layers)

    # Write one GeoTIFF for this year-month
    outf <- file.path(dest_dir,glue("SML_",cmodel,"_{ymk}.tif"))
    writeRaster(rstack, outf, overwrite = TRUE,
                wopt = list(gdal = "COMPRESS=LZW", datatype = "FLT4S"))

    cat(glue("Wrote {outf} with {nlyr(rstack)} variables.\n"))
  }

  # Second the CC

  CC <- readRDS(paste0("/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/Trendy.",cmodel,".S2.CC.pantropical.v13.RDS")) %>%
    ungroup()

  all_na_cols = sapply(CC, \(x) all(is.na(x)))
  CC = CC[!all_na_cols]

  vars <- CC %>%
    dplyr::select(-any_of(c("lon","lat","year","month"))) %>%
    colnames()

  df <- CC %>%
    ungroup() %>%
    filter(year >= 1980) %>%
    filter(abs(lat) <= 25) %>%
    mutate(ym = sprintf("%04d_%02d", year, month)) %>%
    arrange(year, month)

  ym_list <- unique(df %>%
                      mutate(ym = sprintf("%04d_%02d", year, month)) %>%
                      pull(ym))

  for (ymk in ym_list) {

    dsub <- df %>%
      mutate(ym = sprintf("%04d_%02d", year, month)) %>%
      filter(ym == ymk) %>%
      mutate(
        lon = as.numeric(lon),
        lat = as.numeric(lat))

    # Build a template grid from lon/lat
    lon_vals <- sort(unique(dsub$lon))
    lat_vals <- sort(unique(dsub$lat))
    ext <- ext(min(lon_vals), max(lon_vals), min(lat_vals), max(lat_vals))
    dx <- min(diff(lon_vals))
    dy <- min(diff(lat_vals))
    tem <- rast(ext, resolution = c(dx, dy), crs = "EPSG:4326")

    # Rasterize each variable
    layers <- lapply(vars, function(v) {
      vsub <- vect(dsub[, c("lon","lat",v)],
                   geom = c("lon","lat"), crs = "EPSG:4326")
      r <- rasterize(vsub, tem, field = v)
      names(r) <- v
      r
    })

    # Combine all variables into one multilayer raster
    rstack <- do.call(c, layers)

    # Write one GeoTIFF for this year-month
    outf <- file.path(dest_dir,glue("CC_",cmodel,"_{ymk}.tif"))
    writeRaster(rstack, outf, overwrite = TRUE,
                wopt = list(gdal = "COMPRESS=LZW", datatype = "FLT4S"))

    cat(glue("Wrote {outf} with {nlyr(rstack)} variables.\n"))
  }


}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Format.DGVMs.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

