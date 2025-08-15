read.Trendy <- function(ncfile,
                        lat.names = c("latitude","lat","lat_FULL"),
                        lon.names = c("longitude","lon","lon_FULL"),
                        time.names = c("time","time_counter"),
                        variables.names = c("npp"),
                        years2select,
                        lat2select =  NULL,
                        lon2select = NULL){

  # library(ncdf4)
  # library(lubridate)
  # library(reshape2)
  # #
  # ncfile = "/data/gent/vo/000/gvo00074/felicien/TrENDYv12/CARDAMOM_S2_gpp.nc"
  lat.names = c("latitude","lat","lat_FULL")
  lon.names = c("longitude","lon","lon_FULL")
  time.names = c("time","time_counter")
  # variables.names = c("npp")
  # years2select  = c(1960,Inf)
  # lat2select =  c(-20,15)
  # lon2select = c(-15,50)

  nc <- nc_open(ncfile)

  lats <- NULL ; i = 1
  while(is.null(lats) & i <= length(lat.names)){
    lats <- tryCatch(suppressMessages(ncvar_get(nc,lat.names[i])),
                     error = function(e) NULL)
    i = i +1
  }

  lons <- NULL ; i = 1
  while(is.null(lons) & i <= length(lon.names)){
    lons <- tryCatch(suppressMessages(ncvar_get(nc,lon.names[i])),
                     error = function(e) NULL)
    i = i +1
  }

  if (all(lats < 1e-6)){
    delta_lat = 180/length(lats)
    lats <- seq(-90 + delta_lat/2,90 - delta_lat/2,delta_lat)
  }

  if (all(lons < 1e-6)){
    delta_lon = 360/length(lons)
    lons <- seq(0 + delta_lon/2,360 - delta_lon/2,delta_lon)
  }

  lons[lons>180] <- lons[lons>180] -360

  # ncfilin <- ncdf4::nc_open(ncfile)

  # ncdf4::nc_close(ncfilin)

  times <- TrENDY.analyses::nc.get.time.series(f = nc)
  old.times <- times

  if (all(is.na(times))){
    Nyears <- round(length(times)/12)
    years <- sort(rep(1700:(1700+Nyears-1),12))
    months <- rep(1:12,Nyears)
    times <- as.Date(paste0(years,'/',months,'/01'))

    old.times <- times
  }


  years <- lubridate::year(times)
  months <- lubridate::month(times)

  if (all(is.na(times))){
    A <- 11/12+ncvar_get(nc,"time")/12
    years <- floor(A)
    months <- round(1+(A - years)*12)
    times <- as.Date(paste0(years,"/",months,"/","01"))
  }


  # times <- NULL ; i = 1
  #
  # while(is.null(times) & i <= length(time.names)){
  #   times <- tryCatch(suppressMessages(ncvar_get(nc,time.names[i])),
  #                     error = function(e) NULL)
  #
  #   if (!is.null(times)) unit.time <- strsplit(ncatt_get(nc,time.names[i],"units")[["value"]], "\\s+")[[1]]
  #
  #   i = i + 1
  # }
  #
  #


  nc2 <- RNetCDF::open.nc(ncfile)

  tunits <- NULL ; i = 1

  while(is.null(tunits) & i <= length(time.names)){
    tunits <- tryCatch(suppressMessages(RNetCDF::att.get.nc(nc2, time.names[i],'units')),
                       error = function(e) NULL)

    i = i + 1
  }

  if (is.null(tunits)) tunits <- "Unknown"

  RNetCDF::close.nc(nc2)

  abs.times <- TrENDY.analyses::nc.get.abs.time.series(f = nc)

  time.origin <- nc.get.time.origin(f = nc)

  time.split <- strsplit(nc$dim$time$units, " ")[[1]]
  time.res <- time.split[1]
  time.multiplier <- TrENDY.analyses::nc.get.time.multiplier(time.res)


  # if (time.multiplier %in% c(86400,86400*365/12) & lubridate::day(time.origin) == 1){ # hack
  #
  #   warning(paste0("Correcting time for",ncfile))
  #
  #   months <- rep(1:12,length(times)/12)
  #   times <- PCICt::as.PCICt.default(paste0(years,"/",sprintf("%02d",months),"/01"),
  #                                    cal = "gregorian")
  #
  # }

  check.time <- years + (months - 1/2)/12

  if (min(diff(check.time)) == 0){

    months <- rep(1:12,length(times)/12)

    if (!all(table(years) == 12)){
      years <- sort(rep(unique(years),12))
    }

    new.times <- PCICt::as.PCICt.default(paste0(years,"/",sprintf("%02d",months),"/01"),
                                     cal = "gregorian")

    # warning(paste("Correcting time for",ncfile,"\r\n",
    #               "Previous times:",times,"\r\n",
    #               "New times:",new.times,"\r\n"))

    times <- new.times

    if(length(times) != length(old.times)){

      Delta <- -(length(old.times) - length(times))
      times <- times[(Delta + 1) : length(times)]
      years <- lubridate::year(times)
      months <- lubridate::month(times)

    }

  }


  round.years <- floor(years)

  select <- which(round.years >= years2select[1],round.years <= years2select[2])

  times.selected <- times[select]
  years.selected <- years[select]
  months.selected <- months[select]
  abs.times.selected <- abs.times[select]

  if (!is.null(lat2select)){
    select.lat <- which(lats>=lat2select[1] & lats<=lat2select[2])
    lats <- lats[select.lat]
  } else{
    select.lat <- 1:length(lats)
  }

  if (!is.null(lon2select)){
    select.lon <- which(lons>=lon2select[1] & lons<=lon2select[2])
    lons <- lons[select.lon]
  } else{
    select.lon <- 1:length(lons)
  }

  # Read values

  values <- NULL ; i = 1
  while(is.null(values) & i <= length(variables.names)){
    values <- tryCatch(ncvar_get(nc,variables.names[i],
                                 start = c(min(select.lon),min(select.lat),min(select)),
                                 count = c(length(select.lon),length(select.lat),length(select))),
                       error = function(e) NULL)
    i = i +1
  }


  nc_close(nc)

  cdf <- melt(values) %>%
    rename(lon = Var1,
           lat = Var2,
           time = Var3) %>%
    mutate(lon = lons[lon],
           lat = lats[lat],
           year = years.selected[time],
           month = months.selected[time],
           abs.time = abs.times.selected[time],
           time = times.selected[time],
           time.unit = as.character(tunits),
           ) %>%
    group_by(lat,lon) %>%
    # mutate(year =  round.years[select]) %>%
    filter(!is.na(value))

  return(cdf)

}
