rm(list = ls())

library(dplyr)
library(raster)
library(terra)

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
land.frac.msk[land.frac.msk>=0.25] <- 1


e <- extent(-180,180,-25,25)

df.prop <- df.ts <-
  data.frame()

all.maps <- list()
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
  r.product.rspld.msk <- terra::mask(r.product.rspld,land.frac.msk)

  r.mean <- mean(r.product.rspld.msk,na.rm = TRUE)
  names(r.mean) <- cproduct

  all.maps[[iproduct]] <- r.mean

  names(r.product.rspld.msk) <- paste0(years,'_',months)

  E <- ext(r.product.rspld.msk)
  band_means <- global(r.product.rspld.msk, "mean",na.rm = TRUE)

  df.ts <- bind_rows(df.ts,
                     data.frame(product = cproduct,
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
                       data.frame(product = cproduct,

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
        "./outputs/GPP.products.spec.RDS")
saveRDS(df.ts,
        "./outputs/GPP.products.ts.RDS")
writeRaster(mean(rast(all.maps), na.rm = TRUE),
            "./outputs/GPP.products.m.tif", overwrite=TRUE)
writeRaster(rast(all.maps),
            "./outputs/GPP.products.all.tif", overwrite=TRUE)

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Compile.GPP.products.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/


