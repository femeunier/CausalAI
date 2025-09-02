# First sftp

rm(list = ls())

library(raster)
library(dplyr)
library(CausalAI)

years <- 1980:2024
years.ref <- 1900:2100
months <- 1:12

raster.grid = rast(raster(extent(-179.75, 179.75,
                                 -24.75, 24.75),
                          res = 1,
                          crs = "+proj=longlat +datum=WGS84"))
land.frac <- rast(rasterFromXYZ(readRDS("./outputs/landFrac.RDS")))
land.frac.rspld <- terra::resample(land.frac,raster.grid)
land.frac.msk <- land.frac.rspld
land.frac.msk[land.frac.msk < 0.25] <- 0
land.frac.msk[land.frac.msk>=0.25] <- 1


dest_dir <- "/data/gent/vo/000/gvo00074/ED_common_data/met/GLEAM/"

e <- extent(-180,180,-25,25)

for (cyear in years){

  file <- file.path(dest_dir,paste0("SMs_",cyear,"_GLEAM_v4.2a_MO.nc"))
  r <- stack(file)

  for (cmonth in months){

    print(paste0(cyear,"-",cmonth))

    op <- file.path(dest_dir,
                    paste0("GLEAM_SMs_",cyear,"_",sprintf("%02d",cmonth),".tif"))

    if (file.exists(op)){
      next()
    }

    cr <- crop(r[[cmonth]],e)
    names(cr) <- "top.sml"

    writeRaster(cr,
                op,
                overwrite = TRUE,
                wopt = list(gdal = "COMPRESS=LZW", datatype = "FLT4S"))

  }
}

files <- list.files(dest_dir,
                    pattern = "GLEAM_SMs.*tif$",
                    full.names = TRUE)
all.years <- as.numeric(unlist(lapply(
  strsplit(tools::file_path_sans_ext(basename(files)),"\\_"),
           "[[",3)))
all.months <- as.numeric(unlist(lapply(
  strsplit(tools::file_path_sans_ext(basename(files)),"\\_"),
           "[[",4)))

r <- terra::rast(files)
r.rspld <- terra::resample(r,rast(raster.grid))
r <- terra::mask(project(r.rspld,crs(raster.grid)),
                 land.frac.msk)

names(r) <- paste0(all.years,"_",all.months)

dates <- as.Date(paste0(all.years,"/",all.months,"/01"))
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
            file.path(dest_dir,paste0("top.sml.","gleam",".tif")),
            overwrite = TRUE,
            wopt = list(gdal = "COMPRESS=LZW", datatype = "FLT4S"))

writeRaster(r - anoms,
            file.path(dest_dir,paste0("top.smltrends.","gleam",".tif")),
            overwrite = TRUE,
            wopt = list(gdal = "COMPRESS=LZW", datatype = "FLT4S"))

writeRaster(r_detr,
            file.path(dest_dir,paste0("top.smldetrended.","gleam",".tif")),
            overwrite = TRUE,
            wopt = list(gdal = "COMPRESS=LZW", datatype = "FLT4S"))

writeRaster(anoms,
            file.path(dest_dir,paste0("top.smlanomaly.","gleam",".tif")),
            overwrite = TRUE,
            wopt = list(gdal = "COMPRESS=LZW", datatype = "FLT4S"))

# scp /home/femeunier/Documents/projects/CausalAI/scripts/download.GLEAM_SMs.R hpc:/data/gent/vo/000/gvo00074/felicien/R
