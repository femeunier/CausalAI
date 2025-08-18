# Madani
# https://search.earthdata.nasa.gov/search/granules?p=C2764742564-ORNL_CLOUD&pg[0][v]=f&pg[0][gsk]=-start_date&q=Global_Monthly_GPP_1789&ac=true&tl=931003200!5!!

rm(list = ls())

library(ncdf4)
library(raster)
library(lubridate)

dir <- "/data/gent/vo/000/gvo00074/felicien/GPP_data/Madani"
ncfile <- file.path(dir,
                    "gross_primary_productivity_monthly_1982-2016.nc4")

e <- extent(-180,180,-25,25)

nc <- nc_open(ncfile)

times <- as.Date(ncvar_get(nc,"time"),
                 origin = "1982-01-01 00:00:00")
years <- year(times)
months <- month(times)

nc_close(nc)
r <- brick(ncfile)

for (i in seq(1, length(times))){

  print(paste0(i,"/",length(times)))

  cr <- r[[i]]

  writeRaster(crop(cr,e)/1000* ifelse(lubridate::leap_year(paste0(years[i],"/01/01")),
                          366,365),
              file.path(dir,
                        paste0('GPP.Madani.',years[i],
                               ".",sprintf("%02d",months[i]),".tif")),
              options=c('TFW=YES'),
              overwrite = TRUE)
}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Download.Madani.GPP.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

