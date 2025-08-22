rm(list = ls())

library(terra)
library(dplyr)
library(glue)

dest_dir <- "/data/gent/vo/000/gvo00074/felicien/R/outputs/CRUJRA/"


for (decade in c(1980,1990,2000,2010,2020)){

  CRUJRA <- readRDS(paste0("./outputs/monthly.climate.pantropical.",decade,".RDS"))
  clim_vars <- CRUJRA %>%
    dplyr::select(-any_of(c("lon","lat","year","month","fdir"))) %>%
    colnames()

  df <- CRUJRA %>%
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
        lat = as.numeric(lat)
      )

    # Build a template grid from lon/lat
    lon_vals <- sort(unique(dsub$lon))
    lat_vals <- sort(unique(dsub$lat))
    ext <- ext(min(lon_vals), max(lon_vals), min(lat_vals), max(lat_vals))
    dx <- min(diff(lon_vals))
    dy <- min(diff(lat_vals))
    tem <- rast(ext, resolution = c(dx, dy), crs = "EPSG:4326")

    # Rasterize each variable
    layers <- lapply(clim_vars, function(v) {
      vsub <- vect(dsub[, c("lon","lat",v)],
                   geom = c("lon","lat"), crs = "EPSG:4326")
      r <- rasterize(vsub, tem, field = v)
      names(r) <- v
      r
    })

    # Combine all variables into one multilayer raster
    rstack <- do.call(c, layers)

    # Write one GeoTIFF for this year-month
    outf <- file.path(dest_dir,glue("climate_{ymk}_CRUJRA.tif"))
    writeRaster(rstack, outf, overwrite = TRUE,
                wopt = list(gdal = "COMPRESS=LZW", datatype = "FLT4S"))

    cat(glue("Wrote {outf} with {nlyr(rstack)} variables.\n"))
  }
}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Format.CRUJRA.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
