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

Nrun.max.per.job <- 20

main.config <- list(lags = 12,
                    initial = 200,
                    horizon = 12,
                    global.suffix = "DGVM",
                    step = 12,
                    skip = 11,
                    fac.CC = 86400*365,
                    threshold = 0.1,
                    climate.location = "/data/gent/vo/000/gvo00074/felicien/R/outputs/CRUJRA/climate",
                    raster.grid = raster(extent(-179.75, 179.75,
                                                -24.75, 24.75),
                                         res = 1,
                                         crs = "+proj=longlat +datum=WGS84"),
                    x_var = c("tmp","tmin","tmax",
                              "dswrf","vpd","CO2",
                              "pre","top.sml"),
                    y_var = "gpp",

                    year.min = 1980,
                    year.max = 2050,

                    Grid = expand.grid(
                      nrounds = c(200, 600, 1200),
                      max_depth = c(3, 6, 12),
                      eta = c(0.03, 0.1),
                      gamma = c(0),
                      colsample_bytree = c(0.8),
                      min_child_weight = c(1),
                      subsample = c(0.8)),

                    time2save = 600)

models <- c("CABLE-POP","CLASSIC","CLM6.0",
            "E3SM","JSBACH","JULES","LPJ-GUESS",
            "LPJmL","LPX-Bern","VISIT")
models <- models[c(3)]

raster.grid <- main.config[["raster.grid"]]

land.frac <- rasterFromXYZ(readRDS("./outputs/landFrac.RDS"))
land.frac.rspld <- raster::resample(land.frac,raster.grid)
all.df.lon.lat <- as.data.frame(land.frac.rspld,xy = TRUE) %>%
  rename(lon = x, lat = y) %>%
  filter(value > 0.25) %>%
  filter(abs(lat) < 25) %>%
  mutate(lon_lat = paste0(lon,"_",lat)) %>%
  ungroup() %>%
  mutate(id = 1:n())

all.lons_lats <- all.df.lon.lat$lon_lat

dir.name <- "/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/Granger/"
dir.create(dir.name,showWarnings = FALSE)

mainconfig.file <- file.path(dir.name,
                             paste0("main.config.",main.config[["global.suffix"]],".RDS"))

saveRDS(main.config,
        mainconfig.file)

list_dir <- list() ; job.names <- c()

for (cmodel in models){

  print(paste0(cmodel))

  dir.create(file.path(dir.name,cmodel),showWarnings = FALSE)

  #######################################################################################################
  # We first check what is done already


  files <- list.files(file.path("./outputs/Granger/",cmodel),
                      pattern = "^QoF.*Granger.*.RDS",
                      full.names = TRUE)
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
                      mutate(model = cmodel),
                    error = function(e) NULL)

    if (is.null(cdf)) next()
    if (nrow(cdf) == 0) next()

    df.runs <- bind_rows(df.runs,
                         cdf)
  }
  finished.all.lons.lat <- df.runs[["lon_lat"]]

  all.lons_lats <- all.lons_lats[!(all.lons_lats %in% finished.all.lons.lat)]
  df.lon.lat <- all.df.lon.lat %>%
    filter(lon_lat %in% all.lons_lats) %>%
    ungroup() %>%
    mutate(id = 1:n())
  Ntot.run <- length(all.lons_lats)

  #######################################################################################################

  model.config <- main.config
  model.config[["SWC.location"]] <- paste0("/data/gent/vo/000/gvo00074/felicien/R/outputs/DGVM/",cmodel,"/SML_",cmodel)
  model.config[["CC.location"]] <- paste0("/data/gent/vo/000/gvo00074/felicien/R/outputs/DGVM/",cmodel,"/CC_",cmodel)
  model.config[["dest.dir"]] <- file.path(dir.name,cmodel)
  model.config[["name"]] <- cmodel


  modelconfig.file <- file.path(dir.name,cmodel,
                                paste0("config.",cmodel,".RDS"))

  saveRDS(model.config,
          modelconfig.file)

  compt <- (max.compt+1)
  for (istart in seq(1,Ntot.run,Nrun.max.per.job)){
    lons_lats <- df.lon.lat %>%
      filter(id %in% c(istart:(istart + Nrun.max.per.job -1))) %>%
      pull(lon_lat)

    location.file <- file.path(file.path(dir.name, cmodel),
                               paste0("location.",compt,".RDS"))
    saveRDS(lons_lats,
            location.file)

    suffix <- paste0(cmodel,"_",compt)

    write.Granger.script(dir.name = file.path(dir.name, cmodel),
                         file.name = paste0("Rscript_",suffix,".R"),
                         config.location = modelconfig.file,
                         coord.location = location.file,
                         cmodel,
                         suffix = suffix)

    cjobname <- paste0("job_",suffix,".pbs")
    ED2scenarios::write_jobR(file = file.path(dir.name,cmodel,cjobname),
                             nodes = 1,ppn = 16,mem = 100,walltime = 12,
                             prerun = "ml purge ; ml R-bundle-Bioconductor/3.20-foss-2024a-R-4.4.2",
                             CD = file.path(dir.name,cmodel),
                             Rscript = paste0("Rscript_",suffix,".R"))
    job.names <- c(job.names,cjobname)
    list_dir[[suffix]] = file.path(dir.name,cmodel)

    compt <- compt + 1

  }
}

dumb <- write_bash_submission(file = file.path(getwd(),
                                               "All.Granger.sh"),
                              list_files = list_dir,
                              job_name = job.names)

# scp /home/femeunier/Documents/projects/CausalAI/scripts/continue.Granger.grid.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
