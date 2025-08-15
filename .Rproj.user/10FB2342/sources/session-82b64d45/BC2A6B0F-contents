run.Granger <- function(config.file){

  config <- readRDS(config.file)

  cmodel <- config[["cmodel"]]
  lags <- config[["lags"]]
  initial <- config[["initial"]]
  horizon <- config[["horizon"]]
  step <- config[["step"]]
  skip <- config[["skip"]]
  threshold <- config[["threshold"]]
  x_var <- config[["x_var"]]
  y_var <- config[["y_var"]]
  Grid <- config[["Grid"]]

  time2save <- config[["time2save"]]

  lat.max <- config[["lat.max"]]
  lat.min <- config[["lat.min"]]
  lon.max <- config[["lon.max"]]
  lon.min <- config[["lon.min"]]
  year.max <- config[["year.max"]]
  year.min <- config[["year.min"]]

  dest.dir <- config[["dest.dir"]]

  if (requireNamespace("future", quietly = TRUE)) {
    Ncores <- as.numeric(future::availableCores())
    future::plan("multisession", workers = Ncores)
  }

  suffix <- paste0(cmodel,
                   "_lats",lat.min,".",lat.max,
                   "_lons",lon.min,".",lon.max)


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

  climate <- readRDS(paste0("/kyukon/data/gent/vo/000/gvo00074/felicien/R/data/grid.",cmodel,".JRA.v13.RDS")) %>%
    mutate(lat = round(lat,digits = 3),
           lon = round(lon,digits = 3)) %>%
    left_join(monthly_df,
              by = c("year","month")) %>%
    ungroup()

  msl.file <- paste0("/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/MSL.grid.",cmodel,".RDS")
  msl.ts <- readRDS(msl.file) %>%
    mutate(year = year(time),
           month = month(time)) %>%
    dplyr::select(lon,lat,year,month,
                  top.sml,tot.sml) %>%
    mutate(lat = round(lat,digits = 3),
           lon = round(lon,digits = 3)) %>%
    ungroup() %>%
    mutate(lon = case_when(lon > 180 ~ lon - 360,
                           TRUE ~ lon))

  CC <- readRDS(paste0("/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/Trendy.",cmodel,".S2.CC.pantropical.v13.RDS")) %>%
    dplyr::select(lon,lat,year,month,gpp) %>%
    mutate(lat = round(lat,digits = 3),
           lon = round(lon,digits = 3)) %>%
    ungroup()

  # Merge all datasets
  all <- climate %>%
    left_join(msl.ts,
              by = c("lon","lat","year","month")) %>%
    left_join(CC %>%
                mutate(gpp = as.numeric(gpp)*86400*365),
              by = c("lon","lat","year","month")) %>%
    dplyr::select(any_of(c("lon","lat","year","month",
                           x_var,
                           y_var))) %>%
    mutate(lon_lat = paste0(lon,"_",lat))

  all.select <- all %>%
    filter(lat <= lat.max, lat > lat.min) %>%
    filter(lon >= lon.min, lon < lon.min) %>%
    filter(year >= year.min, year <= year.max) %>%
    na.omit()

  lons_lats <- all.select %>%
    pull(lon_lat) %>%
    unique()

  ##################################################################
  # Timer
  hour_start <- Sys.time()

  scale_z <- function(df, mu, sdv) {
    as.data.frame(mapply(function(z, m, s) (z - m) / s, df, mu, sdv))
  }

  #######################################################################
  # Main loop

  if (restart){

    df.QoF <-
      all.X.test <-
      all.test <-
      all.SHAP <-
      data.frame()
  } else {

    df.QoF <- tryCatch(readRDS(file.path(dest.dir,paste0("QoF.Granger_",suffix,".RDS"))),
                       error = function(e) data.frame())
    all.X.test <- tryCatch(readRDS(file.path(dest.dir,paste0("All.test.XGBoosts_",suffix,".RDS"))),
                           error = function(e) data.frame())
    all.SHAP <- tryCatch(readRDS(file.path(dest.dir,paste0("All.SHAP_",suffix,".RDS"))),
                         error = function(e) data.frame())
    all.test <- tryCatch(readRDS(file.path(dest.dir,paste0("All.X.test.",suffix,".RDS"))),
                         error = function(e) data.frame())

    lons_lats <- lons_lats[!(lons_lats %in% unique(df.QoF$lon_lat))]
  }

  for (ilon.lat in seq(1,length(lons_lats))){

    clon.lat <- lons_lats[ilon.lat]
    print(ilon.lat/length(lons_lats))

    df <- all.select %>%
      filter(lon_lat == clon.lat) %>%
      dplyr::select(-any_of(c("lon","lat","lon_lat",
                              "year","month")))

    if(all(df[[y_var]] == 0) |
       all(is.na(df[[y_var]]))){
      next()
    }

    if(all(df[[y_var]] < gpp.threshold)){
      next()
    }

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

    fit <- tryCatch(tune_xgb_with_caret(train = data.matrix(dfl.train),
                                        y = as.numeric(y.train),
                                        grid = Grid,
                                        target = "gpp",
                                        lags = lags,
                                        initial = initial, horizon = horizon, skip = skip),
                    error = function(e) NULL)

    if (is.null(fit)) next()

    bestTune <- fit$bestTune
    bestModel <- fit$finalModel

    y.pred <- predict(bestModel,
                      dfl.test[,fit$finalModel$feature_names])
    RMSE <- caret::RMSE(y.pred, y.test)
    RSQ <- rsq_vec(as.numeric(y.pred), as.vector(y.test))
    rBias <- mean(100*(y.test - y.pred)/y.test,
                  na.rm = T)

    df.QoF <- bind_rows(df.QoF,
                        data.frame(lon_lat = clon.lat,
                                   RMSE,
                                   Rsq = RSQ,
                                   rBias,
                                   mean.y = mean(df[[y_var]],na.rm = TRUE),
                                   sd.y = sd(df[[y_var]],na.rm = TRUE),
                                   model = cmodel))

    all.test <- bind_rows(all.test,
                          data.frame(pred = y.pred,
                                     obs = y.test,
                                     model = cmodel,
                                     lon_lat = clon.lat))

    all.X.test <- bind_rows(all.X.test,
                            (as.data.frame(dfl.test)) %>%
                              mutate(model = cmodel,
                                     lon_lat = clon.lat))

    run <- tryCatch(ml_granger_all_causes(df, dfl,
                                          target = "gpp", lags = lags,
                                          initial = initial, horizon = horizon,
                                          step = step,
                                          bestTune = bestTune),
                    error = function(e) NULL)


    if (is.null(run)) next()

    shap_df <- run$shap_lags %>%
      as.data.frame()

    all.SHAP <- bind_rows(all.SHAP,
                          shap_df %>%
                            mutate(model = cmodel,
                                   lon_lat = clon.lat))

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

      hour_start <- Sys.time()

    }
  }
}
