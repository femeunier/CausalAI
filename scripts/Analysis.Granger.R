rm(list = ls())

library(dplyr)
library(tidyr)

models <- c("CABLE-POP","CLASSIC","CLM6.0",
            "E3SM","JSBACH","JULES","LPJ-GUESS",
            "LPJmL","LPX-Bern","VISIT")
models <- c("CABLE-POP","CLM6.0","JSBACH")

models <- c("FLUXCOM_ANN","FLUXCOM_RF","FLUXCOM_HB_RF","FLUXCOM-X",
            "GOSIF","Zhou","GLASS","Sun","Bi",
            "Madani","Zhang","VOD","NIR","Zheng","FLUXSAT",
            "MODIS")
models <- c("FLUXSAT","CABLE-POP")

df.QoF <- all.test <- all.SHAP <- all.results <-
  data.frame()

for (cmodel in models){

  print(cmodel)

  # Qof
  files <- list.files(file.path("./outputs/Granger/",cmodel),
                      pattern = "^QoF.*Granger.*.RDS",
                      full.names = TRUE)

  for (cfile in files){
    cQoF <- tryCatch(readRDS(cfile) %>%
                       mutate(model = cmodel),
                     error = function(e) NULL)

    if (is.null(cQoF)) next()
    if (nrow(cQoF) == 0) next()

    df.QoF <- bind_rows(df.QoF,
                        cQoF %>%
                          mutate(lon = as.numeric(sub("\\_.*", "", lon_lat)),
                                 lat = as.numeric(sub(".*\\_", "", lon_lat)),
                                 model_lon_lat = paste0(model,"_",lon_lat)))

  }

  # Test
  files <- list.files(file.path("./outputs/Granger/",cmodel),
                      pattern = "^All.test.XGBoosts.*Granger.*.RDS",
                      full.names = TRUE)

  for (cfile in files){
    call.test <- tryCatch(readRDS(cfile) %>%
                            mutate(model = cmodel),
                          error = function(e) NULL)

    if (is.null(call.test)) next()
    if (nrow(call.test) == 0) next()

    all.test <- bind_rows(all.test,
                          call.test %>%
                            mutate(lon = as.numeric(sub("\\_.*", "", lon_lat)),
                                   lat = as.numeric(sub(".*\\_", "", lon_lat)),
                                   model_lon_lat = paste0(model,"_",lon_lat)))

  }

  # Shap
  files <- list.files(file.path("./outputs/Granger/",cmodel),
                      pattern = "^All.SHAP.*Granger.*.RDS",
                      full.names = TRUE)

  for (cfile in files){
    cSHAP <- tryCatch(readRDS(cfile) %>%
                        mutate(model = cmodel),
                      error = function(e) NULL)

    if (is.null(cSHAP)) next()
    if (nrow(cSHAP) == 0) next()

    all.SHAP <- bind_rows(all.SHAP,
                         cSHAP %>%
                           mutate(lon = as.numeric(sub("\\_.*", "", lon_lat)),
                                  lat = as.numeric(sub(".*\\_", "", lon_lat)),
                                  model_lon_lat = paste0(model,"_",lon_lat)))

  }


  # results
  files <- list.files(file.path("./outputs/Granger/",cmodel),
                      pattern = "^All.results.*Granger.*.RDS",
                      full.names = TRUE)

  for (cfile in files){
    cresult <- tryCatch(readRDS(cfile) %>%
                          mutate(model = cmodel),
                        error = function(e) NULL)

    if (is.null(cresult)) next()
    if (nrow(cresult) == 0) next()

    all.results <- bind_rows(all.results,
                             cresult %>%
                               mutate(lon = as.numeric(sub("\\_.*", "", lon_lat)),
                                   lat = as.numeric(sub(".*\\_", "", lon_lat)),
                                   model_lon_lat = paste0(model,"_",lon_lat)))

  }
}

saveRDS(df.QoF,
        "./outputs/All.QoF.Granger.RDS")
saveRDS(all.test,
        "./outputs/All.test.Granger.RDS")
saveRDS(all.SHAP,
        "./outputs/All.SHAP.Granger.RDS")
saveRDS(all.results,
        "./outputs/All.results.Granger.RDS")

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Analysis.Granger.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

