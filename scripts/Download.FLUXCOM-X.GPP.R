rm(list = ls())

# https://meta.icos-cp.eu/collections/AYj7-lwcdCLnBXJDoscxQZou

library(raster)

dir <- "/data/gent/vo/000/gvo00074/felicien/GPP_data/FLUXCOM-X/"

files <- list.files(dir,pattern = "^GPP_.*.nc$",full.names = TRUE)
years <- as.numeric(unlist(lapply(strsplit(basename(files),"\\_"),"[[",2)))

e <- extent(-180,180,-25,25)

for (ifile in seq(1,length(files))){
  cfile <- files[ifile]

  print(cfile)

  r <- stack(cfile)

  for (imonth in seq(1,12)){

    cr <- crop(r[[imonth]],e)/1000*
      ifelse(lubridate::leap_year(paste0(years[ifile],"/01/01")),
             366,365)

    print(mean(as.vector(cr),na.rm = TRUE))
    writeRaster(cr,
                file.path(dir,
                          paste0("GPP.FLUXCOM-X.",years[ifile],".",sprintf("%02d",imonth),".tif")),
                options=c('TFW=YES'),
                overwrite = TRUE)
  }
}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Download.FLUXCOM-X.GPP.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

