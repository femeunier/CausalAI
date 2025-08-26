rm(list = ls())

library(dplyr)
library(raster)
library(terra)
library(lubridate)

products <- c("FLUXCOM_ANN","FLUXCOM_RF","FLUXCOM_HB_RF","FLUXCOM-X",
              "GOSIF","Zhou","GLASS","Sun","Bi",
              "Madani","Zhang","VOD","NIR","Zheng","FLUXSAT",
              "MODIS") # "Zheng"

dirs <- c("FLUXCOM_RS+METEO","FLUXCOM_RS+METEO","FLUXCOM_RS+METEO","FLUXCOM-X",
          "GOSIF.GPP","Zhou","GLASS","Sun","Bi",
          "Madani","Zhang","VOD.GPP","NIR.GPP","Zheng","FluxSat",
          "MODIS_GPP") # "Zheng"

main.dir <- '/data/gent/vo/000/gvo00074/felicien/GPP_data'

raster.grid = rast(raster(extent(-179.75, 179.75,
                                 -24.75, 24.75),
                          res = 1,
                          crs = "+proj=longlat +datum=WGS84"))
land.frac <- rast(rasterFromXYZ(readRDS("./outputs/landFrac.RDS")))
land.frac.rspld <- terra::resample(land.frac,raster.grid)
land.frac.msk <- land.frac.rspld
land.frac.msk[land.frac.msk < 0.25] <- 0
land.frac.msk[land.frac.msk >= 0.25] <- 1


e <- extent(-180,180,-25,25)
years.ref <- 1981:2010

for (iproduct in seq(1,length(products))){
  cproduct <- products[iproduct]

  print(paste0(cproduct,"-",iproduct,"/",length(products)))

  cdir <- file.path(main.dir,dirs[iproduct])

  files <- list.files(cdir,
                      pattern = paste0("^GPP.",
                                       cproduct,
                                       ".*.tif$"),
                      full.names = TRUE)

  years <- as.numeric(lapply(strsplit(basename(files),"\\."),"[[",3))
  months <- as.numeric(lapply(strsplit(basename(files),"\\."),"[[",4))

  r.product <- rast(files)

  r.product.rspld <- terra::resample(r.product,rast(raster.grid))
  r <- terra::mask(project(r.product.rspld,crs(raster.grid)),
                   land.frac.msk)

  cc.years <- as.numeric(unlist(lapply(strsplit(tools::file_path_sans_ext(basename(files)),"_|\\."),"[[",3)))
  cc.months <- as.numeric(unlist(lapply(strsplit(tools::file_path_sans_ext(basename(files)),"_|\\."),"[[",4)))

  pos <- 3
  while (all(is.na(cc.years))| (max(cc.years) < 20)){
    cc.years <- as.numeric(unlist(lapply(strsplit(tools::file_path_sans_ext(basename(files)),"_|\\."),"[[",pos)))
    cc.months <- as.numeric(unlist(lapply(strsplit(tools::file_path_sans_ext(basename(files)),"_|\\."),"[[",pos+1)))
    pos <- pos + 1
  }

  dates <- as.Date(paste0(cc.years,"/",cc.months,"/01"))

  yrs <- format(dates, "%Y")
  mons_all <- month(dates)
  ref.pos <- yrs %in% years.ref
  ref <- r[[ref.pos]]

  mons <- mons_all[ref.pos]
  clim <- tapp(ref, mons, mean, na.rm = TRUE)

  clim_all <- clim[[mons_all]]
  anoms <- r - clim_all

  writeRaster(anoms,
              file.path(cdir,paste0("gppanomaly.",cproduct,".tif")),
              overwrite = TRUE,
              wopt = list(gdal = "COMPRESS=LZW", datatype = "FLT4S"))

  writeRaster(r,
              file.path(cdir,paste0("gpp.",cproduct,".tif")),
              overwrite = TRUE,
              wopt = list(gdal = "COMPRESS=LZW", datatype = "FLT4S"))

}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Format.GPP.products.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

