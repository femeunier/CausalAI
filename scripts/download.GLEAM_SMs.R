# First sftp

rm(list = ls())

library(raster)
library(dplyr)

years <- 1980:2024
months <- 1:12

dest_dir <- "/data/gent/vo/000/gvo00074/ED_common_data/met/GLEAM/"

e <- extent(-180,180,-25,25)

for (cyear in years){

  file <- file.path(dest_dir,paste0("SMs_",cyear,"_GLEAM_v4.2a_MO.nc"))
  r <- stack(file)

  for (cmonth in months){

    print(paste0(cyear,"-",cmonth))

    cr <- crop(r[[cmonth]],e)
    names(cr) <- "top.sml"

    writeRaster(cr,
                file.path(dest_dir,
                              paste0("GLEAM_SMs_",cyear,"_",sprintf("%02d",cmonth),".tif")),
                overwrite = TRUE,
                wopt = list(gdal = "COMPRESS=LZW", datatype = "FLT4S"))

  }
}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/download.GLEAM_SMs.R hpc:/data/gent/vo/000/gvo00074/felicien/R
