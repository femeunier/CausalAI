# https://figshare.com/articles/dataset/2001-2022_global_gross_primary_productivity_dataset_using_an_ensemble_model_based_on_random_forest/24417649/2

rm(list = ls())

library(raster)

dir <- "/data/gent/vo/000/gvo00074/felicien/GPP_data/Zhou"
files <- list.files(dir,pattern = "*.nc",recursive = TRUE,full.names = TRUE)

e <- extent(-180,180,-25,25)

for (cfile in files){

  cyear <- as.numeric(substr(basename(cfile),8,11))
  cmonth <- as.numeric(substr(basename(cfile),12,13))

  print(paste0(cyear,'-',cmonth))

  r <- raster(cfile)

  dest.file <- file.path(dir,
                         paste0('GPP.Zhou.',cyear,
                                ".",sprintf("%02d",cmonth),".tif"))

  cr <- t(flip(r, 2))
  extent(cr) <- extent(-180, 180, -90, 90)
  crs(cr) <- "EPSG:4326"

  writeRaster(mean(crop(cr,e))*0.001/1000* ifelse(lubridate::leap_year(paste0(cyear,"/01/01")),
                                            366,365),
              dest.file,
              options=c('TFW=YES'),
              overwrite = TRUE)

}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Download.Zhou.GPP.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

