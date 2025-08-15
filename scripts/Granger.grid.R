rm(list = ls())

library(CausalAI)

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
               Grid <- expand.grid(
                 nrounds = c(200, 600, 1200),
                 max_depth = c(3, 6),
                 eta = c(0.03, 0.1),
                 gamma = c(0),
                 colsample_bytree = c(0.8),
                 min_child_weight = c(1),
                 subsample = c(0.8)),
               time2save = 600)

Global.lat.min <- -23 ; Global.lat.max <- 23 ; Delta_lat <- 5
Global.lon.min <- -15 ; Global.lon.max <- 60 ; Delta_lon <- 5

models <- c("CABLE-POP","CLASSIC","CLM6.0",
            "E3SM","JSBACH","JULES","LPJ-GUESS",
            "LPJmL","LPX-Bern","VISIT")

dir.name <- "/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/Granger/"
dir.create(dir.name,showWarnings = FALSE)
mainconfig.file <- file.path(dir.name,"main.config.RDS")

saveRDS(main.config,
        mainconfig.file)

for (cmodel in models){

  dir.create(file.path(dir.name,cmodel),showWarnings = FALSE)

  for (clat in seq(Global.lat.min,Global.lat.max,Delta_lat)){
    for (clon in seq(Global.lon.min,Global.lon.max,Delta_lon)){

      lat.min <- clat ; lat.max <- lat.min + Delta_lat
      lon.min <- clon ; lon.max <- lon.min + Delta_lon

      suffix <- paste0(cmodel,
                       "_lats",lat.min,".",lat.max,
                       "_lons",lon.min,".",lon.max)

      stop()

      write.Granger.script(dir.name = file.path(dir.name,cmodel),
                           file.name = paste0("Rscript_",suffix,".R"),
                           config.location = mainconfig.file,
                           cmodel,
                           lat.min,lat.max,lon.min,lon.max,year.min,year.max)

    }
  }
}





###############################################################
# Load data

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Granger.grid.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

