rm(list = ls())

library(CausalAI)
library(ED2scenarios)
library(dplyr)
library(purrr)

###############################################################
# Settings

main.config <- list(lags = 12,
               initial = 240,
               horizon = 12,
               step = 12,
               skip = 11,
               threshold = 0.1,
               restart = TRUE,
               x_var = c("tmp","tmin","tmax","dswrf","VPD","CO2","pre","top.sml"),
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

Global.lat.min <- -23 ; Global.lat.max <- 23 ; Delta_lat <- 10
Global.lon.min <- -15 ; Global.lon.max <- 60 ; Delta_lon <- 10

models <- c("CABLE-POP","CLASSIC","CLM6.0",
            "E3SM","JSBACH","JULES","LPJ-GUESS",
            "LPJmL","LPX-Bern","VISIT")

dir.name <- "/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/Granger/"
dir.create(dir.name,showWarnings = FALSE)
mainconfig.file <- file.path(dir.name,"main.config.RDS")

saveRDS(main.config,
        mainconfig.file)


list_dir <- list() ; job.names <- c()

Nsim <- 0
Nsim.max <- 50

for (cmodel in models){

  print(cmodel)

  new.job.file <- TRUE
  init <- TRUE

  dir.create(file.path(dir.name,cmodel),showWarnings = FALSE)

  cCC <- readRDS(paste0("/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/Trendy.",cmodel,".S2.CC.pantropical.v13.RDS")) %>%
    dplyr::select(lon,lat,year,month,gpp) %>%
    filter(year >= main.config$year.min,
           year <= main.config$year.max) %>%
    mutate(gpp = gpp*86400*365) %>%
    distinct()

  for (clat in seq(Global.lat.min,Global.lat.max,Delta_lat)){
    for (clon in seq(Global.lon.min,Global.lon.max,Delta_lon)){

      lat.min <- clat ; lat.max <- lat.min + Delta_lat
      lon.min <- clon ; lon.max <- lon.min + Delta_lon

      cdf <- cCC %>%
        filter(lat >= lat.min, lat < lat.max) %>%
        filter(lon >= lon.min, lon < lon.max) %>%
        na.omit()

      if (nrow(cdf) == 0) next()

      cdf.run <- cdf %>%
        group_by(lon,lat) %>%
        summarise(run = !all(gpp < main.config[["threshold"]]),
                  .groups = "keep") %>%
        filter(run)

      if (nrow(cdf.run) == 0) next()

      suffix <- paste0(cmodel,
                       "_lats",lat.min,"_",lat.max,
                       "_lons",lon.min,"_",lon.max)

      write.Granger.script(dir.name = file.path(dir.name,cmodel),
                           file.name = paste0("Rscript_",suffix,".R"),
                           config.location = mainconfig.file,
                           cmodel,
                           lat.min,lat.max,
                           lon.min,lon.max)


      Nsim <- Nsim + nrow(cdf.run)

      if (Nsim > Nsim.max){
        new.job.file <- TRUE
        Nsim <- 0
      } else {
        new.job.file <- FALSE
      }

      if (init | new.job.file){
        cjobname <- paste0("job_",suffix,".pbs")
        ED2scenarios::write_jobR(file = file.path(dir.name,cmodel,cjobname),
                                 nodes = 1,ppn = 16,mem = 100,walltime = 12,
                                 prerun = "ml purge ; ml R-bundle-Bioconductor/3.20-foss-2024a-R-4.4.2",
                                 CD = file.path(dir.name,cmodel),
                                 Rscript = paste0("Rscript_",suffix,".R"))
        job.names <- c(job.names,cjobname)
        list_dir[[suffix]] = file.path(dir.name,cmodel)

        if (init){
          init <- FALSE
        }

      } else {

        write(paste0("Rscript ",paste0("Rscript_",suffix,".R")),
              file=file.path(dir.name,cmodel,cjobname),
              append=TRUE)
      }
    }
  }
}

dumb <- write_bash_submission(file = file.path(getwd(),
                                               "All.Granger.sh"),
                              list_files = list_dir,
                              job_name = job.names)

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Granger.grid.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
