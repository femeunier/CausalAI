# VOD.GPP
# https://researchdata.tuwien.ac.at/records/1k7aj-bdz35

rm(list = ls())

library(ncdf4)
library(raster)
library(lubridate)
library(dplyr)

dir <- "/data/gent/vo/000/gvo00074/felicien/GPP_data/VOD.GPP"
ncfile <- file.path(dir,
                    "VODCA2GPP_v1.nc")

e <- extent(-180,180,-25,25)

nc <- nc_open(ncfile)

times <- as.Date(ncvar_get(nc,"time"),
                 origin = "1858-11-17 00:00:00")
years <- year(times)
months <- month(times)

nc_close(nc)
r <- stack(ncfile)

df <- data.frame(year = years,
                 month = months) %>%
  distinct()

overwrite <- TRUE

for (i in seq(1,nrow(df))){

  print(paste0(i,"/",nrow(df)))
  cyear <- df$year[i] ; cmonth <- df$month[i]

  pos <- which((years == cyear) & (months == cmonth))
  cr <- r[[c(pos)]]

  dest.file <- file.path(dir,
                        paste0('GPP.VOD.',cyear,
                               ".",sprintf("%02d",cmonth),".tif"))

  if (!overwrite & file.exists(dest.file)){
    next
  }

  writeRaster(mean(crop(cr,e))/1000* ifelse(lubridate::leap_year(paste0(cyear,"/01/01")),
                                      366,365),
              dest.file,
              options=c('TFW=YES'),
              overwrite = TRUE)
}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Download.VOD.GPP.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

