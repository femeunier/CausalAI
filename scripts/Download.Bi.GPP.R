# https://datadryad.org/dataset/doi:10.5061/dryad.dfn2z352k

rm(list = ls())

library(dplyr)
library(lubridate)
library(raster)

years <- c(2017:2020)
dest_dir <- "/data/gent/vo/000/gvo00074/felicien/GPP_data/Bi/"

e <- extent(-180,180,-25,25)

setwd(dest_dir)

for (iyear in seq(1,length(years))){

  cyear <- years[iyear]
  dest.file <- file.path(dest_dir,
                         paste0(cyear,".zip"))

  if (!file.exists(dest.file)) next()

  system2("unzip",
          c(dest.file,
            paste0("'",cyear,"/Month/*","'")))

  files <- list.files(file.path(dest_dir,cyear,"Month"),
                      pattern = "^GPP_v21.*.tif$",full.names = TRUE)
  files <- files[grepl(cyear,files)]

  months <- as.numeric(unlist(lapply(strsplit(basename(tools::file_path_sans_ext(files)),"\\_"),"[[",4)))

  for (cmonth in months){

    print(paste0(cyear,"-",cmonth))

    cfile <- file.path(dest_dir,
                       cyear,"Month",
                       paste0("GPP_v21_",cyear,"_",sprintf("%02d",cmonth),".tif"))

    Ndays <- lubridate::days_in_month(paste0(cyear,"/",sprintf("%02d",cmonth),"/01"))

    all.r <- raster::crop(raster::raster(cfile),e)
    cr <- all.r/10/Ndays/1000*
      ifelse(lubridate::leap_year(paste0(cyear,"/01/01")),
             366,365)
    cr[cr == 0] <- NA

    writeRaster(cr,
                file.path(dest_dir,
                          paste0("GPP.Bi.",cyear,".",sprintf("%02d",cmonth),".tif")),
                options=c('TFW=YES'),
                overwrite = TRUE)

  }

  system2("rm",
          c("-rf",
            cyear))

  system2("rm",
          c(paste0(cyear,".zip")))

}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Download.Bi.GPP.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

