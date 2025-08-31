run.Granger <- function(config.file){

  config <- readRDS(config.file)

  global.suffix <- config[["global.suffix"]]
  suffix <- config[["suffix"]]
  lags <- config[["lags"]]
  name <- config[["name"]]
  initial <- config[["initial"]]
  horizon <- config[["horizon"]]
  skip.num <- config[["skip"]]
  step <- config[["step"]]

  threshold <- config[["threshold"]]
  x_var <- config[["x_var"]]
  y_var <- config[["y_var"]]
  Grid <- config[["Grid"]]

  year.min <- config[["year.min"]]
  year.max <- config[["year.max"]]

  time2save <- config[["time2save"]]
  lons_lats <- config[["lons_lats"]]

  fac.CC <- config[["fac.CC"]]

  if (is.null(fac.CC)){
    fac.CC <- 86400*365
  }

  raster.grid <- config[["raster.grid"]]
  SWC.location <- config[["SWC.location"]]
  CC.location <- config[["CC.location"]]
  climate.location <- config[["climate.location"]]

  dest.dir <- config[["dest.dir"]]

  CO2 <- read.table("/kyukon/data/gent/vo/000/gvo00074/felicien/R/data/global_co2_ann_1700_2024.txt") %>%
    rename(year = V1,
           CO2 = V2)

  monthly_df <- expand.grid(
    month = 1:12,
    year = CO2$year) %>%
    arrange(year, month) %>%
    mutate(year_decimal = year + (month - 0.5) / 12)

  monthly_df$CO2 <- approx(
    x = CO2$year,
    y = CO2$CO2,
    xout = monthly_df$year_decimal)$y


  climate.files <- list.files(path = dirname(climate.location),
                              pattern = paste0("^",
                                               basename(climate.location),
                                               ".*.tif$"),
                              full.names = TRUE)
  climate <- rast(climate.files)
  names(climate) <- gsub("[0-9]","", names(climate))
  names(climate) <- gsub(tolower(name),"",tolower(names(climate)))
  names(climate) <- gsub("[._]+$", "",names(climate))
  climate.years <- as.numeric(unlist(lapply(strsplit(tools::file_path_sans_ext(basename(climate.files)),"_|\\."),"[[",2)))
  climate.months <- as.numeric(unlist(lapply(strsplit(tools::file_path_sans_ext(basename(climate.files)),"_|\\."),"[[",3)))


  msl.files <- list.files(path = dirname(SWC.location),
                              pattern = paste0("^",
                                               basename(SWC.location),
                                               ".*.tif$"),
                          full.names = TRUE)
  msl.rspld <- rast(msl.files)
  cnames <- names(msl.rspld)

  msl.years <- as.numeric(unlist(lapply(strsplit((basename(cnames)),"_|\\."),"[[",1)))
  msl.months <- as.numeric(unlist(lapply(strsplit((basename(cnames)),"_|\\."),"[[",2)))

  names(msl.rspld) <- rep("top.sml",nlyr(msl.rspld))

  cc.files <- list.files(path = dirname(CC.location),
                          pattern = paste0("^",
                                           basename(CC.location),
                                           ".*.tif$"),
                         full.names = TRUE)
  cc.rspld <- rast(cc.files)
  cnames <- names(cc.rspld)

  cc.years <- as.numeric(unlist(lapply(strsplit((basename(cnames)),"_|\\."),"[[",1)))
  cc.months <- as.numeric(unlist(lapply(strsplit((basename(cnames)),"_|\\."),"[[",2)))

  names(cc.rspld) <- rep(y_var,nlyr(cc.rspld))

  ###################################################################
  # We make sure all grids are aligned

  climate.rspld <- terra::resample(climate,rast(raster.grid),method = "bilinear")

  ##################################################################
  # Timer
  hour_start <- Sys.time()

  scale_z <- function(df, mu, sdv) {
    as.data.frame(mapply(function(z, m, s) (z - m) / s, df, mu, sdv))
  }

  #######################################################################
  # Main loop

  df.QoF <-
    all.X.test <-
    all.test <-
    all.SHAP <-
    all.results <-
    data.frame()

  for (ilon.lat in seq(1,length(lons_lats))){

    skip <- FALSE

    clon.lat <- lons_lats[ilon.lat]

    clon <- as.numeric(strsplit(clon.lat,"\\_")[[1]][1])
    clat <- as.numeric(strsplit(clon.lat,"\\_")[[1]][2])

    print(paste0(ilon.lat/length(lons_lats),"-",clon,"/",clat))

    # We exctract
    temp.climate <- terra::extract(climate.rspld,
                                  terra::vect(data.frame(lon = clon,
                                                        lat = clat),
                                              geom = c("lon", "lat")))

    cdf.climate <- data.frame(variable = colnames(temp.climate),
                              value = as.numeric(temp.climate)) %>%
      dplyr::filter(variable != "ID") %>%
      ungroup() %>%
      arrange(variable) %>%
      mutate(year = as.numeric(rep(climate.years,length(unique(variable)))),
             month = as.numeric(rep(climate.months,length(unique(variable))))) %>%
      pivot_wider(names_from = "variable",
                  values_from = "value")

    temp.msl <- terra::extract(msl.rspld,
                                   terra::vect(data.frame(lon = clon,
                                                          lat = clat),
                                               geom = c("lon", "lat")))

    cdf.msl <- data.frame(variable = colnames(temp.msl),
                              value = as.numeric(temp.msl)) %>%
      dplyr::filter(variable != "ID") %>%
      ungroup() %>%
      arrange(variable) %>%
      mutate(year = as.numeric(rep(msl.years,length(unique(variable)))),
             month = as.numeric(rep(msl.months,length(unique(variable))))) %>%
      pivot_wider(names_from = "variable",
                  values_from = "value")

    temp.cc <- terra::extract(cc.rspld,
                               terra::vect(data.frame(lon = clon,
                                                      lat = clat),
                                           geom = c("lon", "lat")))

    cdf.cc <- data.frame(variable = colnames(temp.cc),
                          value = as.numeric(temp.cc)) %>%
      dplyr::filter(variable != "ID") %>%
      ungroup() %>%
      arrange(variable) %>%
      mutate(year = as.numeric(rep(cc.years,length(unique(variable)))),
             month = as.numeric(rep(cc.months,length(unique(variable))))) %>%
      mutate(value = value*fac.CC) %>%
      pivot_wider(names_from = "variable",
                  values_from = "value")

    # We merged them all

    all <- cdf.climate %>%
      left_join(cdf.msl,
                by = c("year","month")) %>%
      left_join(cdf.cc,
                by = c("year","month")) %>%
      left_join(monthly_df,
                by = c("year","month")) %>%
      dplyr::filter(year >= year.min,
             year <= year.max) %>%
      dplyr::select(any_of(c(x_var,y_var))) %>%
      na.omit()

    df <- all %>%
      dplyr::select(-any_of(c("lon","lat","lon_lat",
                              "year","month")))

    if((nrow(df) < 60)){
      outcome = "Timeseries.too.short"
      skip <- TRUE
    } else if(all(df[[y_var]] == 0) |
       all(is.na(df[[y_var]]))){

      outcome = "not.run.all.zero"
      skip <- TRUE
    } else if(all(abs(df[[y_var]]) < threshold)){

      outcome = "not.run.all.small"
      skip <- TRUE
    }

    if (!skip){

      mu <- sapply(df, mean) ; mu[y_var] <- 0
      sdv <- sapply(df, sd) ; sdv[y_var] <- 1

      df <- scale_z(df, mu, sdv)
      dfl <- make_lags(df, max_lag = lags) %>%
        na.omit()

      smp_size <- floor(0.8 * nrow(dfl))
      train_ind <- 1:smp_size

      df.train <- df[train_ind,c(x_var,y_var)]
      dfl.train <- as.matrix(dfl[train_ind,
                                 setdiff(colnames(dfl), y_var)])
      y.train <- as.matrix(dfl[train_ind, y_var])

      df.test <- df[-train_ind,c(x_var,y_var)]
      dfl.test <- as.matrix(dfl[-train_ind, setdiff(colnames(dfl), y_var)])
      y.test <- as.matrix(dfl[-train_ind, y_var])


      if (initial > nrow(df.train) | is.null(initial)){
        initial <- floor(0.7*nrow(df.train))
      }

      fit <- tryCatch(tune_xgb_with_caret(train = data.matrix(dfl.train),
                                          y = as.numeric(y.train),
                                          grid = Grid,
                                          target = y_var,
                                          lags = lags,
                                          initial = initial, horizon = horizon, skip = skip.num),
                      error = function(e) NULL)

      if (!is.null(fit)){
        bestTune <- fit$bestTune
        bestModel <- fit$finalModel

        y.pred <- predict(bestModel,
                          dfl.test[,fit$finalModel$feature_names])
        RMSE <- caret::RMSE(y.pred, y.test)
        RSQ <- rsq_vec(as.numeric(y.pred), as.vector(y.test))
        rBias <- mean(100*(y.test - y.pred)/y.test,
                      na.rm = T)
      } else {
        RMSE <- NA_real_
        RSQ <- NA_real_
        rBias <- NA_real_
        outcome <- "Bug.when.fitting"
        skip <- TRUE
      }
    } else {
      RMSE <- NA_real_
      RSQ <- NA_real_
      rBias <- NA_real_
      skip <- TRUE
    }


    if (!skip){

      run <- tryCatch(ml_granger_all_causes(df, dfl,
                                            target = y_var, lags = lags,
                                            initial = initial, horizon = horizon,
                                            step = step,
                                            bestTune = bestTune),
                      error = function(e) NULL)


      if (is.null(run)){

        outcome <- "Bug.with.causality"
        skip <- TRUE
      } else {

        outcome <- "fine"

      }
    }


    df.QoF <- bind_rows(df.QoF,
                        data.frame(lon_lat = clon.lat,
                                   outcome,
                                   RMSE,
                                   Rsq = RSQ,
                                   rBias,
                                   mean.y = mean(df[[y_var]],na.rm = TRUE),
                                   sd.y = sd(df[[y_var]],na.rm = TRUE)))

    if (!skip){
      all.test <- bind_rows(all.test,
                            data.frame(pred = y.pred,
                                       obs = y.test,
                                       lon_lat = clon.lat))

      all.X.test <- bind_rows(all.X.test,
                              (as.data.frame(dfl.test)) %>%
                                mutate(lon_lat = clon.lat))

      results <- run$results %>%
        as.data.frame()

      all.results <- bind_rows(all.results,
                               results %>%
                                 mutate(lon_lat = clon.lat))

      shap_df <- run$shap_lags %>%
        as.data.frame()

      all.SHAP <- bind_rows(all.SHAP,
                            shap_df %>%
                              mutate(lon_lat = clon.lat))

    }

    elapsed <- as.numeric(difftime(Sys.time(), hour_start, units = "secs"))

    if (elapsed >= time2save) {

      saveRDS(df.QoF,
              file.path(dest.dir,paste0("QoF.Granger_",suffix,".RDS")))
      saveRDS(all.test,
              file.path(dest.dir,paste0("All.test.XGBoosts.Granger_",suffix,".RDS")))
      saveRDS(all.SHAP,
              file.path(dest.dir,paste0("All.SHAP.Granger_",suffix,".RDS")))
      saveRDS(all.X.test,
              file.path(dest.dir,paste0("All.X.test.Granger_",suffix,".RDS")))
      saveRDS(all.results,
              file.path(dest.dir,paste0("All.results.Granger_",suffix,".RDS")))

      hour_start <- Sys.time()

    }
  }

  saveRDS(df.QoF,
          file.path(dest.dir,paste0("QoF.Granger_",suffix,".RDS")))
  saveRDS(all.test,
          file.path(dest.dir,paste0("All.test.XGBoosts.Granger_",suffix,".RDS")))
  saveRDS(all.SHAP,
          file.path(dest.dir,paste0("All.SHAP.Granger_",suffix,".RDS")))
  saveRDS(all.X.test,
          file.path(dest.dir,paste0("All.X.test.Granger_",suffix,".RDS")))
  saveRDS(all.results,
          file.path(dest.dir,paste0("All.results.Granger_",suffix,".RDS")))

}
