# Zhang
# https://springernature.figshare.com/articles/dataset/Monthly_GPP_at_0_05_degree/5048113?backTo=%2Fcollections%2FA_global_moderate_resolution_dataset_of_gross_primary_production_of_vegetation_for_2000-2016%2F3789814&file=8945542

rm(list = ls())

library(raster)
library(lubridate)

dir <- "/data/gent/vo/000/gvo00074/felicien/GPP_data/Zhang/"
files <- list.files(dir, pattern = "GPP.VPM.*.tif$", full.names = FALSE)

years <- as.numeric(lapply(strsplit(files,"\\."),"[[",3))
months <- as.numeric(sub("M","",lapply(strsplit(files,"\\."),"[[",4)))

e <- extent(-180,180,-25,25)

for (ifile in seq(1,length(files))){

  print(paste0(ifile,"/",length(files)))

  cyear <- years[ifile] ; cmonth <- months[ifile]

  Ndays <- as.numeric(days_in_month(paste0(cyear,
                                           "/",
                                           sprintf("%02d",cmonth),
                                           "/01")))

  r <- raster(file.path(dir,files[ifile]))
  writeRaster(crop(r,e)/Ndays/1000* ifelse(lubridate::leap_year(paste0(cyear,"/01/01")),
                          366,365),
              file.path(dir,
                        paste0('GPP.Zhang.',cyear,
                               ".",sprintf("%02d",cmonth),".tif")),
              options=c('TFW=YES'),
              overwrite = TRUE)
}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Download.Zhang.GPP.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

