rm(list = ls())

library(R.utils)
library(raster)
library(ggplot2)
library(dplyr)
library(akima)
library(RColorBrewer)
library(pracma)
library(lubridate)

dir <- "/data/gent/vo/000/gvo00074/felicien/GPP_data/GOSIF.GPP/"

years <- 2000:2024
months <- 1:12

SF <- (0.01)        # Scaling factor

first <- TRUE

e <- extent(-180,180,-25,25)

all.df <- data.frame()
for (iyear in seq(1,length(years))){
  for (imonth in seq(1,length(months))){
    year <- years[iyear]
    month <- months[imonth]

    print(paste(year,"-",month))

    file.name <- paste0("GOSIF_GPP_",year,".M",sprintf("%02d",month),"_Mean.tif")
    file <- file.path(dir,file.name)

    gz.name <- paste0(file.name,".gz")
    tmp.f <- file.path(dir,gz.name)

    f <- paste0('http://data.globalecology.unh.edu/data/GOSIF-GPP_v2/Monthly/Mean/',gz.name)
    if (!any(file.exists(c(file.path(dir,file.name),
                           tmp.f)))) {

      test <- tryCatch(download.file(f, tmp.f, mode = 'wb'),
                       error = function(e) NULL)

      if(is.null(test)){
        next()
      }

    }

    if (file.exists(file.path(dir,paste0(file.name,".tmp")))){
      t <- file.remove(file.path(dir,paste0(file.name,".tmp")))
    }

    if (!file.exists(file.path(dir,file.name))){
      gunzip(tmp.f, remove = TRUE)
    }

    craster <- raster(file.path(dir,file.name))

    if(first){
      craster.ref <- craster
      first <- FALSE
    }

    craster[craster >= 32766] <- NA_real_       # Water bodies, oceans

    craster <- raster::resample(craster,craster.ref)
    Ndays <- as.numeric(lubridate::days_in_month(as.Date(paste0(year,"/",sprintf("%02d",month),"/01"))))
    craster <- craster*SF/Ndays # gC/day

    dumb <- file.remove(tmp.f)

    all.raster.cr <- (crop(craster,e))
    cdf <- as.data.frame(all.raster.cr, xy = TRUE) %>%
      rename(lon = x,
             lat = y,
             value = starts_with("GOSIF")) %>%
      rename(value = starts_with("layer"))

    cdf.sum <- cdf %>%
      mutate(lon = round(lon,digits = 3),
             lat = round(lon,digits = 3)) %>%
      mutate(lon.lat = paste0(lon,".",lat)) %>%
      filter(!is.na(value)) %>%
      summarise(N = n(),
                value.m = mean(value,na.rm = TRUE))

    all.df <- bind_rows(all.df,
                        cdf.sum %>%
                          mutate(year = years[iyear],
                                 month = months[imonth]))

  }
}

saveRDS(all.df,
        file.path(dir,paste0("GOSIF.GPP.ts.RDS")))

# scp /home/femeunier/Documents/projects/CausalAI/scripts/download.GOSIF.GPP.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
