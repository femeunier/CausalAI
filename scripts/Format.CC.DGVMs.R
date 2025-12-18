rm(list = ls())

library(ncdf4)
library(dplyr)
library(lubridate)
library(TrENDY.analyses)
library(reshape2)
library(raster)
library(terra)
library(glue)

models <- TrENDY.analyses::get.model.names.TRENDY(version = "v13")
models <- rev(models)[4:9]

dx_user <- NULL   # e.g., 0.25
dy_user <- NULL   # e.g., 0.25
bbox     <- NULL  # c(xmin, xmax, ymin, ymax) or NULL to use data-tight bbox

main_dir <- "/data/gent/vo/000/gvo00074/felicien/R/outputs/DGVM"

wrap_lon <- function(lon) {
  lon <- as.numeric(lon)
  lon <- ifelse(lon > 180, lon - 360, lon)
  return(lon)
}

for (imodel in seq(1,length(models))){

  cmodel <- models[imodel]

  dest_dir <- file.path(main_dir,cmodel)
  dir.create(dest_dir,showWarnings = FALSE)

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
        lon = wrap_lon(lon),
        lat = as.numeric(lat)
      ) %>%
      filter(is.finite(lon), is.finite(lat))

    stopifnot(nrow(dsub) > 0)

    # 2) Choose grid spacing (median of unique diffs is robust to irregular sampling)
    if (is.null(dx_user)) {
      lonu <- sort(unique(round(dsub$lon, 6)))
      dx   <- suppressWarnings(median(diff(lonu), na.rm = TRUE))
      if (!is.finite(dx) || dx <= 0) dx <- 0.25
    } else dx <- dx_user

    if (is.null(dy_user)) {
      latu <- sort(unique(round(dsub$lat, 6)))
      dy   <- suppressWarnings(median(diff(latu), na.rm = TRUE))
      if (!is.finite(dy) || dy <= 0) dy <- 0.25
    } else dy <- dy_user

    # 3) Build a regular template grid
    if (is.null(bbox)) {
      xmin <- min(dsub$lon, na.rm = TRUE) - dx/2
      xmax <- max(dsub$lon, na.rm = TRUE) + dx/2
      ymin <- min(dsub$lat, na.rm = TRUE) - dy/2
      ymax <- max(dsub$lat, na.rm = TRUE) + dy/2
    } else {
      xmin <- bbox[1]; xmax <- bbox[2]; ymin <- bbox[3]; ymax <- bbox[4]
    }

    # ensure integer number of cells, then re-tighten edges to avoid drift
    ncol <- max(1L, as.integer(round((xmax - xmin)/dx)))
    nrow <- max(1L, as.integer(round((ymax - ymin)/dy)))
    xmax <- xmin + ncol * dx
    ymax <- ymin + nrow * dy

    tem <- rast(ncols = ncol, nrows = nrow,
                xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                crs  = "EPSG:4326")

    # 4) Map points to cell indices (no fp/border loss)
    eps <- .Machine$double.eps * 100
    col_idx <- floor((dsub$lon - xmin + eps)/dx) + 1L               # west→east
    row_idx <- floor((ymax - dsub$lat + eps)/dy) + 1L               # top row = 1

    # keep only points that land inside
    ok <- col_idx >= 1L & col_idx <= ncol & row_idx >= 1L & row_idx <= nrow &
      is.finite(col_idx) & is.finite(row_idx)
    if (!any(ok)) stop("No points fall inside the target grid. Check bbox/dx/dy.")

    col_idx <- col_idx[ok]; row_idx <- row_idx[ok]
    cell_id <- cellFromRowCol(tem, row = row_idx, col = col_idx)

    # 5) Fill each variable layer by aggregating duplicates (mean)
    make_layer <- function(vals, nm) {
      v <- as.numeric(vals)[ok]
      out <- rep(NA_real_, ncell(tem))
      if (any(!is.na(v))) {
        m <- tapply(v, cell_id, mean, na.rm = TRUE)   # change to sum/median if needed
        out[as.integer(names(m))] <- as.numeric(m)
      }
      r <- tem
      values(r) <- out
      names(r) <- nm
      r
    }

    layers <- lapply(vars, function(v) make_layer(dsub[[v]], v))
    rstack <- do.call(c, layers)

    # Write one GeoTIFF for this year-month
    outf <- file.path(dest_dir,glue("CC_",cmodel,"_{ymk}.tif"))
    writeRaster(rstack, outf, overwrite = TRUE,
                wopt = list(gdal = "COMPRESS=LZW", datatype = "FLT4S"))

    cat(glue("Wrote {outf} with {nlyr(rstack)} variables.\n"))
  }
}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Format.CC.DGVMs.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

