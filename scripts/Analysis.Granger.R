rm(list = ls())

library(dplyr)
library(tidyr)

models <- c("CABLE-POP","CLASSIC","CLM6.0",
            "E3SM","JSBACH","JULES","LPJ-GUESS",
            "LPJmL","LPX-Bern","VISIT")

df.QoF <- all.test <- all.SHAP <-
  data.frame()

for (cmodel in models){

  # Qof
  files <- list.files(file.path("./outputs/Granger/",cmodel),
                      pattern = "^QoF.*Granger.*.RDS",
                      full.names = TRUE)

  for (cfile in files){
    cQoF <- readRDS(cfile)

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
    call.test <- readRDS(cfile)

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
    cSHAP <- readRDS(cfile)

    if (nrow(cSHAP) == 0) next()

    all.SHAP <- bind_rows(all.SHAP,
                         cSHAP %>%
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

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Analysis.Granger.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

