rm(list = ls())

library(dplyr)
library(ncdf4)
library(reshape2)
library(ggplot2)
library(lubridate)
library(raster)

# https://figshare.com/articles/dataset/Long-term_1982-2018_global_gross_primary_production_dataset_based_on_NIRv/12981977?file=24736712

first <- TRUE
SF <- 0.001

e <- extent(-180,180,-25,25)

dir <- "/data/gent/vo/000/gvo00074/felicien/GPP_data/NIR.GPP/"

all.df <- data.frame()
for (cyear in seq(1982,2018)){
  for (cmonth in seq(1,12)){

    file <- file.path(dir,cyear,cfile <- paste0("NIRv.GPP.",cyear,sprintf("%02d",cmonth),".v1.nc"))

    print(cfile)

    if (!file.exists(file)){
      next()
    }

    GPP <- raster(file)
    GPP[GPP == -9999] <- NA
    GPP <- GPP*SF

    GPP.corr <- t(flip(GPP, 2))

    extent(GPP.corr) <- extent(-180, 180, -90, 90)
    crs(GPP.corr) <- "EPSG:4326"

    writeRaster(crop(GPP.corr,e)/1000* ifelse(lubridate::leap_year(paste0(cyear,"/01/01")),
                                        366,365),
                file.path(dir,
                          paste0('GPP.NIR.',cyear,
                                 ".",sprintf("%02d",cmonth),".tif")),
                options=c('TFW=YES'),
                overwrite = TRUE)

  }
}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Download.NIR.GPP.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

