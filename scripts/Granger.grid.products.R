rm(list = ls())

library(CausalAI)
library(ED2scenarios)
library(dplyr)
library(purrr)
library(raster)
library(ggplot2)
library(ggthemes)

###############################################################
# Settings

Nrun.max.per.job <- 300

main.config <- list(lags = 12,
                    initial = 240,
                    horizon = 12,
                    global.suffix = "Product",
                    step = 12,
                    skip = 11,
                    fac.CC = 1,
                    threshold = 0.1,
                    climate.location = "/data/gent/vo/000/gvo00074/felicien/R/outputs/CRUJRA/climate",
                    raster.grid = raster(extent(-179.75, 179.75,
                                                -24.75, 24.75),
                                         res = 2,
                                         crs = "+proj=longlat +datum=WGS84"),

                    x_var = c("tmp","tmin","tmax",
                              "dswrf","VPD","CO2",
                              "pre","top.sml"),
                    y_var = "gpp",

                    year.min = 1991,
                    year.max = 2050,

                    Grid = expand.grid(
                      nrounds = c(200, 600, 1200),
                      max_depth = c(3, 6),
                      eta = c(0.03, 0.1),
                      gamma = c(0),
                      colsample_bytree = c(0.8),
                      min_child_weight = c(1),
                      subsample = c(0.8)),

                    time2save = 600)


products <- c("FLUXCOM_ANN","FLUXCOM_RF","FLUXCOM_HB_RF","FLUXCOM-X",
              "GOSIF","Zhou","GLASS","Sun","Bi",
              "Madani","Zhang","VOD","NIR","Zheng","FLUXSAT",
              "MODIS") # "Zheng"

dirs <- c("FLUXCOM_RS+METEO","FLUXCOM_RS+METEO","FLUXCOM_RS+METEO","FLUXCOM-X",
          "GOSIF.GPP","Zhou","GLASS","Sun","Bi",
          "Madani","Zhang","VOD.GPP","NIR.GPP","Zheng","FluxSat",
          "MODIS_GPP") # "Zheng"

main.dir <- '/data/gent/vo/000/gvo00074/felicien/GPP_data'

raster.grid <- main.config[["raster.grid"]]

land.frac <- rasterFromXYZ(readRDS("./outputs/landFrac.RDS"))
land.frac.rspld <- raster::resample(land.frac,raster.grid)
df.lon.lat <- as.data.frame(land.frac.rspld,xy = TRUE) %>%
  rename(lon = x, lat = y) %>%
  filter(value > 0) %>%
  filter(abs(lat)< 25) %>%
  mutate(lon_lat = paste0(lon,"_",lat)) %>%
  ungroup() %>%
  mutate(id = 1:n())

all.lons_lats <- df.lon.lat$lon_lat
Ntot.run <- length(all.lons_lats)

dir.name <- "/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/Granger/"
dir.create(dir.name,showWarnings = FALSE)

mainconfig.file <- file.path(dir.name,
                             paste0("main.config.",main.config[["global.suffix"]],".RDS"))

saveRDS(main.config,
        mainconfig.file)

list_dir <- list() ; job.names <- c()

for (iproduct in seq(1,length(products))){

  cproduct <- products[iproduct]

  print(paste0(cproduct))

  dir.create(file.path(dir.name,cproduct),showWarnings = FALSE)

  product.config <- main.config
  product.config[["SWC.location"]] <- paste0("/data/gent/vo/000/gvo00074/ED_common_data/met/GLEAM/GLEAM_SMs_")
  product.config[["CC.location"]] <- file.path(main.dir,
                                             dirs[iproduct],paste0("GPP.",cproduct))

  product.config[["dest.dir"]] <- file.path(dir.name,cproduct)
  product.config[["name"]] <- cproduct


  productconfig.file <- file.path(dir.name,cproduct,
                                paste0("config.",cproduct,".RDS"))

  saveRDS(product.config,
          productconfig.file)

  compt <- 1
  for (istart in seq(1,Ntot.run,Nrun.max.per.job)){
    lons_lats <- df.lon.lat %>%
      filter(id %in% c(istart:(istart + Nrun.max.per.job -1))) %>%
      pull(lon_lat)

    location.file <- file.path(file.path(dir.name, cproduct),
                               paste0("location.",compt,".RDS"))
    saveRDS(lons_lats,
            location.file)

    suffix <- paste0(cproduct,"_",compt)

    write.Granger.script(dir.name = file.path(dir.name, cproduct),
                         file.name = paste0("Rscript_",suffix,".R"),
                         config.location = productconfig.file,
                         coord.location = location.file,
                         cproduct,
                         suffix = suffix)

    cjobname <- paste0("job_",suffix,".pbs")
    ED2scenarios::write_jobR(file = file.path(dir.name,cproduct,cjobname),
                             nodes = 1,ppn = 16,mem = 100,walltime = 12,
                             prerun = "ml purge ; ml R-bundle-Bioconductor/3.20-foss-2024a-R-4.4.2",
                             CD = file.path(dir.name,cproduct),
                             Rscript = paste0("Rscript_",suffix,".R"))
    job.names <- c(job.names,cjobname)
    list_dir[[suffix]] = file.path(dir.name,cproduct)

    compt <- compt + 1

  }
}

dumb <- write_bash_submission(file = file.path(getwd(),
                                               "All.Granger.products.sh"),
                              list_files = list_dir,
                              job_name = job.names)

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Granger.grid.products.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
