rm(list = ls())

library(ncdf4)
library(dplyr)
library(lubridate)
library(TrENDY.analyses)
library(reshape2)

models <- c("CABLE-POP","CLASSIC","CLM6.0",
            "E3SM","JSBACH","JULES","LPJ-GUESS",
            "LPJmL","LPX-Bern","VISIT")

models <- c("LPX-Bern")

dir <- "/data/gent/vo/000/gvo00074/felicien/TrENDYv13/"

overwrite <- FALSE

lat.names = c("latitude","lat","lat_FULL")
lon.names = c("longitude","lon","lon_FULL")
time.names = c("time","time_counter")

for (imodel in seq(1,length(models))){

  cmodel <- models[imodel]
  print(cmodel)

  op.file <- paste0("./outputs/MSL.grid.",cmodel,".RDS")

  if (file.exists(op.file) & !overwrite){
    next()
  }

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

  msl_dims <- sapply(nc$var$msl$dim, function(x) x$name)
  dim_lengths <- sapply(nc$var$msl$dim, function(x) x$len)

  dim_map <- setNames(msl_dims, msl_dims)
  dim_map[grepl(paste0(lon.names,collapse = "|"), msl_dims, ignore.case = TRUE)] <- "lon"
  dim_map[grepl(paste0(lat.names,collapse = "|"), msl_dims, ignore.case = TRUE)] <- "lat"
  dim_map[grepl(paste0(time.names,collapse = "|"), msl_dims, ignore.case = TRUE)] <- "time"
  dim_map[grepl("smlayer|depth|layer", msl_dims, ignore.case = TRUE)] <- "smlayer"

  dim_order <- unname(dim_map)

  # Rearrange array so that we know the order is: [lon, lat, lev, time]
  target_order <- c("lon", "lat", "smlayer", "time")
  permute_order <- match(target_order, dim_order)

  valid_dims <- !is.na(permute_order)
  permute_order <- permute_order[valid_dims]
  target_order <- target_order[valid_dims]

  # Create named index maps
  dim_indices <- setNames(seq_along(msl_dims), msl_dims)

  # Define extraction ranges dynamically
  start <- rep(1, length(msl_dims))
  count <- rep(-1,length(msl_dims))

  # Adjust ranges for known dimensions
  if ("time" %in% msl_dims) {
    time_pos <- dim_indices["time"]
    start[time_pos] <- min(pos.times)
    count[time_pos] <- length(pos.times)
  }
  if ("lat" %in% msl_dims | "latitude" %in% msl_dims) {
    lat_name <- intersect(msl_dims, lat.names)[1]
    lat_pos <- dim_indices[lat_name]
    start[lat_pos] <- min(pos.lats)
    count[lat_pos] <- length(pos.lats)
  }

  # Extract variable
  data <- ncvar_get(nc, "msl", start = start, count = count)

  data_std <- aperm(data, permute_order)

  top <- data_std[,,1,]
  stock <- apply(data_std,c(1,2,4),sum)

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
          op.file)

  nc_close(nc)

}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Extract.MSL.grid.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/


