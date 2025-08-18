# FluxCOM
# https://www.fluxcom.org/CF-Download/

rm(list = ls())

library(raster)

years <- 1980:2013
dir <- "/data/gent/vo/000/gvo00074/felicien/GPP_data/FLUXCOM_RS+METEO/"

for (year in years){

  cfile <- file.path(dir,
                     paste0("GPP.ANN.CRUNCEPv6.monthly.",year,".nc"))
  r <- brick(cfile)

  for (month in 1:12){

    print(paste0(year,"-",month))

    cr <- r[[month]]/1000*
      ifelse(lubridate::leap_year(paste0(year,"/01/01")),
             366,365)

    writeRaster(cr,
                file.path(dir,
                          paste0('GPP.FLUXCOM.',year,".",sprintf("%02d",month),".tif")),
                options=c('TFW=YES'),
                overwrite = TRUE)

  }
}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Download.FluxCOM.GPP.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

