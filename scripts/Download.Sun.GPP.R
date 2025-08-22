# https://zenodo.org/records/3996814/files/GPP2018.tar.gz?download=1

rm(list = ls())

library(dplyr)
library(lubridate)
library(raster)

years <- c(1981:2018)
dest_dir <- "/data/gent/vo/000/gvo00074/felicien/GPP_data/Sun/"

e <- extent(-180,180,-25,25)

setwd(dest_dir)

for (iyear in seq(12,length(years))){

  cyear <- years[iyear]
  clink <- paste0("https://zenodo.org/records/3996814/files/GPP",cyear,".tar.gz?download=1")
  dest.file <- file.path(dest_dir,
                         paste0("GPP",cyear,".tar.gz"))

  system2("wget",
          c(clink,"-O",dest.file))

  system2("tar",
          c("-xzvf",dest.file))

  files <- list.files(dest_dir,
                      pattern = ".*GPP.5km.*.tif$",full.names = TRUE)
  files <- files[grepl(cyear,files)]

  Pos.date <- ifelse(lapply(strsplit(basename(files[1]),"\\."),"[[",1) == "GPP",
                     3,4)

  cDate <- unlist(lapply(strsplit(basename(files),"\\."),"[[",Pos.date))

  all.years <- as.numeric(substr(cDate,1,4))
  cDoY <- as.numeric(substr(cDate,5,7))
  all.dates <- as.Date(cDoY-1,origin = paste0(all.years,"/01/01"))
  all.months <- month(all.dates)

  months <- sort(unique(all.months))
  for (cmonth in months){

    print(cmonth)
    pos <- which(all.months == cmonth)

    all.r <- raster::crop(raster::stack(files[pos]),e)
    cr <- raster::calc(all.r, fun = mean, na.rm = TRUE)/100/1000*
      ifelse(lubridate::leap_year(paste0(cyear,"/01/01")),
             366,365)
    cr[cr == 0] <- NA

    writeRaster(cr,
                file.path(dest_dir,
                          paste0("GPP.Sun.",cyear,".",sprintf("%02d",cmonth),".tif")),
                options=c('TFW=YES'),
                overwrite = TRUE)

  }

  system2("rm",
          c(dest.file))

  system2("rm",
          c("*GPP.5km.*.tif"))

}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Download.Sun.GPP.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

