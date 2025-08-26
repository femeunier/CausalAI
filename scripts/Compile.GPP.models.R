rm(list = ls())

library(dplyr)
library(raster)
library(terra)

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

df.prop <- df.ts <-
  data.frame()

all.maps <- list()
for (imodel in seq(1,length(models))){
  cmodel <- models[imodel]

  print(paste0(cmodel,"-",imodel,"/",length(models)))

  cdir <- file.path(main.dir,cmodel)

  files <- list.files(cdir,
                      pattern = paste0("^CC_",
                                       cmodel,
                                       ".*.tif$"),
                      full.names = TRUE)

  years <- as.numeric(lapply(strsplit(basename(tools::file_path_sans_ext(files)),"\\_"),"[[",3))
  months <- as.numeric(lapply(strsplit(basename(tools::file_path_sans_ext(files)),"\\_"),"[[",4))

  r.model <- rast(files)

  r.model <- r.model[[which(grepl("gpp",names(r.model)))]]
  r.model.rspld <- terra::resample(r.model,rast(raster.grid))
  r.model.rspld.msk <- terra::mask(r.model.rspld,land.frac.msk)

  r.mean <- mean(r.model.rspld.msk,na.rm = TRUE)
  names(r.mean) <- cmodel

  all.maps[[imodel]] <- r.mean

  names(r.model.rspld.msk) <- paste0(years,'_',months)

  E <- ext(r.model.rspld.msk)
  band_means <- global(r.model.rspld.msk, "mean",na.rm = TRUE)

  df.ts <- bind_rows(df.ts,
                     data.frame(model = cmodel,
                                year = years,
                                month = months,
                                gpp.m = as.numeric(band_means[["mean"]])))

  df.theor <- expand.grid(year =unique(years),
                          month = 1:12) %>%
    arrange(year,month) %>%
    left_join(data.frame(year = years,
                         month = months) %>%
                mutate(present = TRUE) %>%
                distinct(),
              by = c("year","month"))

  df.prop <- bind_rows(df.prop,
                       data.frame(model = cmodel,

                                  xmin = E[1],
                                  xmax = E[2],
                                  ymin = E[3],
                                  ymax = E[4],

                                  N = length(files),
                                  N.theo = length(unique(years))*12,
                                  missing = df.theor %>%
                                    filter(is.na(present)) %>%
                                    mutate(year_month = paste0(year,
                                                               "_",
                                                               month)) %>%
                                    pull(year_month) %>%
                                    paste0(collapse = "|"),

                                  year.min = min(years,na.rm = TRUE),
                                  year.max = max(years,na.rm = TRUE)))

}

saveRDS(df.prop,
        "./outputs/GPP.models.spec.RDS")
saveRDS(df.ts,
        "./outputs/GPP.models.ts.RDS")
writeRaster(mean(rast(all.maps), na.rm = TRUE),
            "./outputs/GPP.models.m.tif", overwrite=TRUE)
writeRaster(rast(all.maps),
            "./outputs/GPP.models.all.tif", overwrite=TRUE)

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Compile.GPP.models.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/


