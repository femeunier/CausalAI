rm(list = ls())

library(dplyr)
library(raster)
library(terra)
library(lubridate)


models <- c("CABLE-POP","CLASSIC","CLM6.0",
            "E3SM","JSBACH","JULES","LPJ-GUESS",
            "LPJmL","LPX-Bern","VISIT")

raster.grid = rast(raster(extent(-179.75, 179.75,
                                 -24.75, 24.75),
                          res = 1,
                          crs = "+proj=longlat +datum=WGS84"))
land.frac <- rast(rasterFromXYZ(readRDS("./outputs/landFrac.RDS")))
land.frac.rspld <- terra::resample(land.frac,raster.grid)
land.frac.msk <- land.frac.rspld
land.frac.msk[land.frac.msk < 0.25] <- 0
land.frac.msk[land.frac.msk>=0.25] <- 1

main.dir <- "/data/gent/vo/000/gvo00074/felicien/R/outputs/DGVM/"

e <- extent(-180,180,-25,25)
years.ref <- 1981:2010

for (imodel in seq(1,length(models))){

  cmodel <- models[imodel]
  print(paste0(cmodel,"-",imodel,"/",length(models)))

  cdir <- file.path(main.dir,cmodel)

  files <- list.files(cdir,
                      pattern = paste0("^SML_",
                                       cmodel,
                                       ".*.tif$"),
                      full.names = TRUE)

  years <- as.numeric(lapply(strsplit(basename(files),"\\_"),"[[",3))
  months <- as.numeric(lapply(strsplit(basename(files),"\\_"),"[[",4))

  r.model <- rast(files)

  r.model.rspld <- terra::resample(r.model,rast(raster.grid))
  r <- terra::mask(project(r.model.rspld,crs(raster.grid)),
                   land.frac.msk)
  r <- r[[names(r) == "top.sml"]]

  cc.years <- as.numeric(unlist(lapply(strsplit(tools::file_path_sans_ext(basename(files)),"\\_"),"[[",3)))
  cc.months <- as.numeric(unlist(lapply(strsplit(tools::file_path_sans_ext(basename(files)),"\\_"),"[[",4)))

  pos <- 3
  while (all(is.na(cc.years))| (max(cc.years) < 20)){
    cc.years <- as.numeric(unlist(lapply(strsplit(tools::file_path_sans_ext(basename(files)),"_|\\."),"[[",pos)))
    cc.months <- as.numeric(unlist(lapply(strsplit(tools::file_path_sans_ext(basename(files)),"_|\\."),"[[",pos+1)))
    pos <- pos + 1
  }

  dates <- as.Date(paste0(cc.years,"/",cc.months,"/01"))

  names(r) <- paste0(cc.years,"_",sprintf("%02d",cc.months))

  writeRaster(r,
              file.path(cdir,paste0("top.sml.",cmodel,".tif")),
              overwrite = TRUE,
              wopt = list(gdal = "COMPRESS=LZW", datatype = "FLT4S"))
}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Format.SML.models.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

