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

Nrun.max.per.job <- 50

products <- c("FLUXSAT","FLUXCOM_ANN","FLUXCOM_RF","FLUXCOM_HB_RF","FLUXCOM-X",
              "GOSIF","Zhou","GLASS","Sun","Bi",
              "Madani","Zhang","VOD","NIR","Zheng",
              "MODIS")[1:12]

dirs <- c("FluxSat","FLUXCOM_RS+METEO","FLUXCOM_RS+METEO","FLUXCOM_RS+METEO","FLUXCOM-X",
          "GOSIF.GPP","Zhou","GLASS","Sun","Bi",
          "Madani","Zhang","VOD.GPP","NIR.GPP","Zheng",
          "MODIS_GPP")[1:12]

main.config <- list(lags = 6,
                    initial = 180,
                    horizon = 3,
                    global.suffix = "Product",
                    step = 12,
                    skip = 11,
                    fac.CC = 1,
                    threshold = 0.02,
                    climate.location = "/data/gent/vo/000/gvo00074/felicien/R/outputs/CRUJRA/climate",
                    raster.grid = raster(extent(-179.75, 179.75,
                                                -23.25, 23.25),
                                         res = 1,
                                         crs = "+proj=longlat +datum=WGS84"),

                    x_var = c("tmp","tmin","tmax",
                              "dswrf","vpd","CO2",
                              "pre","top.sml"),
                    y_var = "gpp",

                    year.min = 1980,
                    year.max = 2050,

                    Grid = tidyr::crossing(
                      eta_nrounds <- data.frame(
                        eta     = c(0.10, 0.05, 0.03),
                        nrounds = c( 400,   800,  1200)
                      ),
                      max_depth        = c(2, 4, 6),     # keep trees fairly shallow for stability
                      min_child_weight = c(1, 3, 5),     # stronger regularization options
                      gamma            = c(0),           # penalize splits a bit in some configs
                      subsample        = c(0.7),         # row subsampling
                      colsample_bytree = c(0.7)),

                    time2save = 600)

main.dir <- '/data/gent/vo/000/gvo00074/felicien/GPP_data'

raster.grid <- main.config[["raster.grid"]]

land.frac <- rasterFromXYZ(readRDS("./outputs/landFrac.RDS"))
land.frac.rspld <- raster::resample(land.frac,raster.grid)

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

  ######################################################################################################

  all.df.lon.lat <- as.data.frame(land.frac.rspld,xy = TRUE) %>%
    rename(lon = x, lat = y) %>%
    filter(value > 0.25) %>%
    filter(abs(lat)< 25) %>%
    mutate(lon_lat = paste0(lon,"_",lat)) %>%
    ungroup() %>%
    mutate(id = 1:n())

  all.lons_lats <- all.df.lon.lat$lon_lat

  dir.create(file.path(dir.name,cproduct),showWarnings = FALSE)

  #######################################################################################################
  # We first check what is done already

  files <- list.files(file.path("./outputs/Granger/",cproduct),
                      pattern = "^QoF.*Granger.*.RDS",
                      full.names = TRUE)
  if (length(files) == 0){
    max.compt <- 0
    df.lon.lat <- all.df.lon.lat
    Ntot.run <- length(all.lons_lats)

  } else {
    max.compt <- as.numeric(unlist(lapply(strsplit(basename(tools::file_path_sans_ext(files)),"\\_"),"[[",3)))
    point <- 3
    while(all(is.na(max.compt))){
      max.compt <- as.numeric(unlist(lapply(strsplit(basename(tools::file_path_sans_ext(files)),"\\_"),"[[",point+1)))
      point <- point + 1
    }
    max.compt <- max(max.compt)

    df.runs <- data.frame()
    for (cfile in files){
      cdf <- tryCatch(readRDS(cfile) %>%
                        mutate(product = cproduct),
                      error = function(e) NULL)

      if (is.null(cdf)) next()
      if (nrow(cdf) == 0) next()

      df.runs <- bind_rows(df.runs,
                           cdf)
    }

    finished.all.lons.lat <- df.runs %>%
      filter(!grepl("bug",tolower(outcome))) %>%
      pull("lon_lat")


    all.lons_lats <- all.lons_lats[!(all.lons_lats %in% finished.all.lons.lat)]

    if (length(all.lons_lats) == 0) next

    df.lon.lat <- all.df.lon.lat %>%
      filter(lon_lat %in% all.lons_lats) %>%
      ungroup() %>%
      mutate(id = 1:n())
    Ntot.run <- length(all.lons_lats)
  }

  ######################################################################

  product.config <- main.config
  product.config[["SWC.location"]] <- paste0("/data/gent/vo/000/gvo00074/ED_common_data/met/GLEAM/top.sml.gleam")
  product.config[["CC.location"]] <- file.path(main.dir,
                                               dirs[iproduct],paste0(main.config[["y_var"]],".",cproduct))

  product.config[["dest.dir"]] <- file.path(dir.name,cproduct)
  product.config[["name"]] <- cproduct

  productconfig.file <- file.path(dir.name,cproduct,
                                  paste0("config.",cproduct,".RDS"))

  saveRDS(product.config,
          productconfig.file)

  compt <- (max.compt+1)
  for (istart in seq(1,Ntot.run,Nrun.max.per.job)){
    lons_lats <- df.lon.lat %>%
      filter(id %in% c(istart:(istart + Nrun.max.per.job -1))) %>%
      pull(lon_lat)

    location.file <- file.path(file.path(dir.name, cproduct),
                               paste0("location.",compt,".RDS"))
    saveRDS(lons_lats,
            location.file)

    suffix <- paste0(cproduct,"_",main.config[["y_var"]],"_",compt)

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

# scp /home/femeunier/Documents/projects/CausalAI/scripts/continue.Granger.grid.products.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
