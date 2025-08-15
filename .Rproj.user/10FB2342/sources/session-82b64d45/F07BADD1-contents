rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(raster)
library(reshape2)
library(rgdal)
library(YGB)
library(TrENDY.analyses)
library(stringr)
library(randomForest)
library(ggpointdensity)

models <- TrENDY.analyses::get.model.names.TRENDY(version = "v12")
models <- models[4:length(models)]


Tropics.sum <- readRDS("./outputs/monthly.climate.global.ERA5.RDS")
CN <- colnames(Tropics.sum)
Var.names <- CN[!(CN %in% c("lat","lon","year","month"))]

all.grids <- data.frame()

overwrite = TRUE
for (cmodel in models){

  model.file <- paste0("./outputs/Trendy.",cmodel,".S2.CC.global.v12.RDS")

  if (!file.exists(model.file)){
    model.file <- paste0("./outputs/Trendy.",cmodel,".S3.CC.global.v12.RDS")
  }

  if (!file.exists(model.file)) next()

  OPfile <- paste0("./data/grid.",cmodel,".ERA5.v12.RDS")

  if (file.exists(OPfile) & !overwrite){next()}

  cdata <- readRDS(model.file) %>%
    dplyr::select(
      where(
        ~!all(is.na(.x))))

  CN <- colnames(cdata)
  cn.exists <- which(c("gpp","npp","nep","nbp") %in% CN)
  if (length(cn.exists) == 0) next()

  cvar <- as.character(c("gpp","npp","nep","nbp")[cn.exists[1]])

  Biomass.Trendy <- cdata %>%
    mutate(model = cmodel) %>%
    na.omit() %>%
    mutate(Continent = Congo.ED2::coord2continent(lon,lon)) %>%
    mutate(model.lat.lon = paste0(model,".",lat,".",lon))

  cdf <- Biomass.Trendy
  ccdf <- (cdf %>%
    ungroup() %>%
    filter(year == year[1],
           month == month[1]) %>%
    dplyr::select(c("lat","lon",cvar)))[,c("lon","lat",cvar)]

  ccdf <- (cdf %>%
             ungroup() %>%
             filter(year == year[1],
                    month == month[1]) %>%
             dplyr::select(c("lat","lon",cvar)))[,c("lon","lat",cvar)]

  craster <- tryCatch(rasterFromXYZ(ccdf),
                      error = function(e) NULL)

  if (is.null(craster)){

    res <- 1e-8
    error <- TRUE

    while (error & res < 100){

      print(res)
      craster <- tryCatch(raster(suppressWarnings(SpatialPixelsDataFrame(points = ccdf[c("lon","lat")],
                                                                         data = ccdf[cvar],
                                                                         tolerance = res))),
                          error = function(e) NULL)

      if (is.null(craster)) {
        res = res*1.01
      } else {
        error <- FALSE
      }

    }

    if (res > 100){
      next()
    }
  }


  cgrid <- data.frame()

  for (cyear in sort(unique(Tropics.sum$year))){

    print(paste0(cmodel," - ",cyear))


    test <- resample.df.all.col(bigdf = Tropics.sum %>%
                                  mutate(model = "ERA5") %>%
                                  filter(year == cyear),

                                raster2resample = craster,
                                var.names = Var.names,
                                NULL)

    cgrid <- bind_rows(cgrid,
                       test %>%
                         mutate(model = cmodel))
  }


  saveRDS(cgrid,
          OPfile)
}

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/generate.all.grids.v12.ERA5.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
