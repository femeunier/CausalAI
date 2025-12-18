rm(list = ls())

library(ncdf4)
library(dplyr)
library(lubridate)
library(TrENDY.analyses)
library(reshape2)

models <- c("CABLE-POP","CLASSIC","CLM6.0",
            "E3SM","JSBACH","JULES","LPJ-GUESS",
            "LPJmL","LPX-Bern","VISIT")
dir <- "/data/gent/vo/000/gvo00074/felicien/TrENDYv13/"

lat.names = c("latitude","lat","lat_FULL")
lon.names = c("longitude","lon","lon_FULL")
time.names = c("time","time_counter")

for (cmodel in models){

  print(cmodel)

  ncfile <- file.path(dir,paste0(cmodel,"_S2_msl.nc"))
  nc <- nc_open(ncfile)

  all.lats <- NULL ; i = 1
  while(is.null(all.lats) & i <= length(lat.names)){
    all.lats <- tryCatch(suppressMessages(ncvar_get(nc,lat.names[i])),
                         error = function(e) NULL)
    i = i +1
  }

  all.lons <- NULL ; i = 1
  while(is.null(all.lons) & i <= length(lon.names)){
    all.lons <- tryCatch(suppressMessages(ncvar_get(nc,lon.names[i])),
                         error = function(e) NULL)
    i = i +1
  }

  all.times <- TrENDY.analyses::nc.get.time.series(nc)
  all.years <- year(all.times)

  pos.times <- which(all.years >= 1901)
  pos.lats <- which(abs(all.lats) < 23.5)

  times <- as.Date(as.character(all.times[pos.times]))
  lats <- all.lats[pos.lats]
  lons <- all.lons

  data <- ncvar_get(nc,
                    "msl",
                    start = c(1,min(pos.lats),1,min(pos.times)),
                    count = c(-1,length(pos.lats),-1,length(pos.times)))

  top <- data[,,1,]
  stock <- apply(data,c(1,2,4),sum)

  top.df <- reshape2::melt(top) %>%
    mutate(time = times[Var3],
           lon = lons[Var1],
           lat = lats[Var2]) %>%
    dplyr::select(lon,lat,time,value) %>%
    filter(!is.na(value))

  stock.df <- reshape2::melt(stock) %>%
    mutate(time = times[Var3],
           lon = lons[Var1],
           lat = lats[Var2]) %>%
    dplyr::select(lon,lat,time,value) %>%
    filter(!is.na(value))

  all.df <- top.df %>%
    rename(top.sml = value)
  all.df$tot.sml <- stock.df$value

  saveRDS(all.df,
          paste0"./outputs/MSL.grid.",cmodel,".RDS"))


}

