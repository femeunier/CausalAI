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

    writeRaster(crop(r[[cmonth]],e),
                file.path(dest_dir,
                          paste0("GLEAM_SMs_",cyear,".",sprintf("%02d",cmonth),".tif")),
                options=c('TFW=YES'),
                overwrite = TRUE)
  }
}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/download.GLEAM_SMs.R hpc:/data/gent/vo/000/gvo00074/felicien/R
