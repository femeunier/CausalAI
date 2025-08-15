ml_granger_xgb <- function(dfl, target, cause, lags = 12,
                           initial = 200, horizon = 12, step = 6,
                           bestTune, verbose = 0) {
  y <- dfl[[target]]

  future::plan("multisession", workers = Ncores)

  lag_cols <- grep("_L[0-9]+$", names(dfl), value = TRUE)
  X_full <- dfl[, lag_cols, drop = FALSE]
  cause_lag_cols <- paste0(cause, "_L", 1:lags)
  X_red  <- X_full[, setdiff(colnames(X_full), cause_lag_cols), drop = FALSE]

  params <- list(
    objective = "reg:squarederror",
    eta = bestTune$eta,
    max_depth = bestTune$max_depth,
    gamma = bestTune$gamma,
    colsample_bytree = bestTune$colsample_bytree,
    min_child_weight = bestTune$min_child_weight,
    subsample = bestTune$subsample,
    nthread = Ncores
  )
  nrounds <- bestTune$nrounds

  n <- nrow(dfl)
  stops <- seq(from = initial, to = n - horizon, by = step)

  err_full <- numeric(0)
  err_red  <- numeric(0)

  # accumulate SHAP mean|contrib| per lag of the cause across folds
  shap_accum <- NULL
  cause_cols_present <- intersect(cause_lag_cols, colnames(X_full))

  for (train_end in stops) {
    tr_idx <- 1:train_end
    te_idx <- (train_end + 1):(train_end + horizon)

    dtrain_full <- xgb.DMatrix(as.matrix(X_full[tr_idx, ]), label = y[tr_idx])
    dtest_full  <- xgb.DMatrix(as.matrix(X_full[te_idx, ]),  label = y[te_idx])

    dtrain_red <- xgb.DMatrix(as.matrix(X_red[tr_idx, ]), label = y[tr_idx])
    dtest_red  <- xgb.DMatrix(as.matrix(X_red[te_idx, ]),  label = y[te_idx])

    mod_full <- xgb.train(params, dtrain_full,
                          nrounds = nrounds,
                          verbose = verbose)
    mod_red  <- xgb.train(params, dtrain_red,
                          nrounds = nrounds,
                          verbose = verbose)

    pred_full <- predict(mod_full, dtest_full)
    pred_red  <- predict(mod_red,  dtest_red)

    e_full <- y[te_idx] - pred_full
    e_red  <- y[te_idx] - pred_red

    err_full <- c(err_full, e_full)
    err_red  <- c(err_red,  e_red)

    # SHAP (predcontrib)
    shap <- predict(mod_full,
                    dtest_full,
                    predcontrib = TRUE)

    shap <- as.data.frame(shap)[, -ncol(shap), drop = FALSE]  # drop BIAS
    if (length(cause_cols_present)) {
      m.shap <- colMeans(shap[, cause_cols_present, drop = FALSE])
      shap_accum <- rbind(shap_accum, m.shap)
    }
  }

  rmse_full <- sqrt(mean(err_full^2))
  rmse_red  <- sqrt(mean(err_red^2))
  improvement <- rmse_red - rmse_full  # >0 => cause lags help

  dm <- tryCatch(forecast::dm.test(err_red, err_full, alternative = "greater", power = 2),
                 error = function(e) NULL)
  pval <- if (is.null(dm)) NA_real_ else dm$p.value
  stat <- if (is.null(dm)) NA_real_ else as.numeric(dm$statistic)

  shap_summary <- NULL
  if (!is.null(shap_accum) && nrow(shap_accum) > 0) {
    shap_mean <- colMeans(shap_accum)
    shap_summary <- tibble(
      feature = names(shap_mean),
      lag = as.integer(str_extract(names(shap_mean), "(?<=_L)\\d+")),
      mean_shap = as.numeric(shap_mean)
    ) %>% arrange(lag)
  }

  list(improvement = improvement, p_value = pval, dm_stat = stat,
       rmse_full = rmse_full, rmse_reduced = rmse_red,
       shap_lag_summary = shap_summary)
}
