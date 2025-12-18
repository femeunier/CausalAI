# FluxCOM
# https://www.fluxcom.org/CF-Download/

rm(list = ls())

library(raster)

years <- 1980:2013
dir <- "/data/gent/vo/000/gvo00074/felicien/GPP_data/FLUXCOM_RS+METEO/"

e <- extent(-180,180,-25,25)

prefix <- c("GPP.ANN.CRUNCEPv6.monthly",
            "GPP.RF.CRUNCEPv6.monthly",
            "GPP_HB.RF.CRUNCEPv6.monthly")
name <- c("FLUXCOM_ANN",
          "FLUXCOM_RF",
          "FLUXCOM_HB_RF")

for (iprefix in seq(1,length(prefix))){

  cprefix <- prefix[iprefix]
  for (year in years){

    cfile <- file.path(dir,
                       paste0(cprefix,".",year,".nc"))
    r <- brick(cfile)
    NAvalue(r) <- -9999

    for (month in 1:12){

      print(paste0(year,"-",month))

      cr <- crop(r[[month]],e)/1000*
        ifelse(lubridate::leap_year(paste0(year,"/01/01")),
               366,365)

      print(mean(as.vector(cr),na.rm = TRUE))
      writeRaster(cr,
                  file.path(dir,
                            paste0("GPP.",name[iprefix],".",year,".",sprintf("%02d",month),".tif")),
                  options=c('TFW=YES'),
                  overwrite = TRUE)

    }
  }
}



# scp /home/femeunier/Documents/projects/CausalAI/scripts/Download.FluxCOM.GPP.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

