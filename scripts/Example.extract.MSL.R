rm(list = ls())

library(ncdf4)
library(dplyr)
library(lubridate)
library(TrENDY.analyses)
library(reshape2)

cmodel <- "CABLE-POP"
dir <- "/data/gent/vo/000/gvo00074/felicien/TrENDYv13/"
ncfile <- file.path(dir,paste0(cmodel,"_S2_msl.nc"))
nc <- nc_open(ncfile)

lat.names = c("latitude","lat","lat_FULL")
lon.names = c("longitude","lon","lon_FULL")
time.names = c("time","time_counter")

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

data <- ncvar_get(nc,"msl",
                  start = c(1,min(pos.lats),1,min(pos.times)),
                  count = c(-1,length(pos.lats),-1,length(pos.times)))

# Manaus
target.lon <- -60.2
target.lat <- -2.5

dist <- expand.grid(lon = lons,
                    lat = lats) %>%
  mutate(
    target.lon,
    target.lat) %>%
  mutate(dist = sqrt((lon - target.lon)^2 + (target.lat - lat)^2)) %>%
  filter(dist == min(dist)) %>%
  slice_head(n = 1)

example <- data[which(lons == as.vector(dist$lon)),
                which(lats == as.vector(dist$lat)),,]

example.df <- data.frame(reshape2::melt(example) %>%
                           mutate(time = times[Var2]) %>%
                           rename(soil.layer = Var1) %>%
                           mutate(lon = dist$lon,
                                  lat = dist$lat) %>%
                           dplyr::select(lon,lat,time,soil.layer,value))

saveRDS(example.df,"./outputs/Example.MSL.RDS")


