ml_granger_xgb0 <- function(dfl, mod_red, train_id, target, cause, lags = 12,
                           initial = 200, horizon = 12, step = 6,
                           bestTune, verbose = 0) {
  y <- dfl[[target]]

  if (requireNamespace("future", quietly = TRUE)) {
    Ncores <- as.numeric(future::availableCores())
    future::plan("multisession", workers = Ncores)
  }

  lag_cols <- grep(paste0(target,"_L[0-9]+$"), names(dfl), value = TRUE)
  X_red  <- dfl[, c(lag_cols), drop = FALSE]
  cause_lag_cols <- paste0(cause, "_L", 1:lags)
  X_full <- dfl[, c(lag_cols,cause_lag_cols), drop = FALSE]

  # split
  n <- nrow(dfl)
  train_id <- sort(unique(train_id))
  test_id  <- setdiff(seq_len(n), train_id)
  if (length(test_id) < 5L) {
    future::plan("sequential")
    return(list(
      improvement = NA_real_, cw_p_value = NA_real_, cw_stat = NA_real_,
      rmse_full = NA_real_, rmse_reduced = NA_real_,
      shap_lag_summary = NULL, n_oos = 0L
    ))
  }

  # --- Fit once on TRAIN ONLY (full vs reduced) --------------------------
  feats_full <- colnames(X_full)
  feats_red  <- colnames(X_red)

  dtrain_full <- xgboost::xgb.DMatrix(as.matrix(X_full[train_id, feats_full, drop = FALSE]),
                                      label = as.numeric(y[train_id]))
  # dtrain_red  <- xgboost::xgb.DMatrix(as.matrix(X_red [train_id, feats_red , drop = FALSE]),
  #                                     label = as.numeric(y[train_id]))

  params <- list(
    objective = "reg:squarederror",
    eta = bestTune$eta,
    max_depth = bestTune$max_depth,
    gamma = bestTune$gamma,
    colsample_bytree = bestTune$colsample_bytree,
    min_child_weight = bestTune$min_child_weight,
    subsample = bestTune$subsample,
    nthread = Ncores,
    tree_method = "hist",
    predictor = "cpu_predictor"
  )
  nrounds <- bestTune$nrounds

  mod_full <- xgboost::xgb.train(params, dtrain_full, nrounds = nrounds, verbose = verbose)
  # mod_red  <- xgboost::xgb.train(params, dtrain_red , nrounds = nrounds, verbose = verbose)

  # --- TEST-ONLY predictions (align to what you trained for) -------------
  # Here we produce standard 1-step-ahead style predictions at the test timestamps.
  # If you want true direct h-step, create y_{t+h} upstream and pass it as `target`,
  # then set h = horizon below. Otherwise keep h = 1 here.
  Xte_full <- as.matrix(X_full[test_id, feats_full, drop = FALSE])
  Xte_red  <- as.matrix(X_red [test_id, feats_red , drop = FALSE])

  f1 <- as.numeric(predict(mod_full, Xte_full))
  f0 <- as.numeric(predict(mod_red , Xte_red ))
  y_oos <- as.numeric(y[test_id])

  # --- Metrics + Clark–West on test only --------------------------------
  rmse_full <- sqrt(mean((y_oos - f1)^2))
  rmse_red  <- sqrt(mean((y_oos - f0)^2))
  improvement <- rmse_red - rmse_full          # >0 => adding cause helps

  # Use h = 1 for these sequential test predictions;
  # set h = horizon *only* if target already encodes y_{t+h}.
  cw <- clark_west_test(y = y_oos, f0 = f0, f1 = f1, h = 1, alternative = "greater")

  # --- SHAP on TEST ONLY (no leakage) -----------------------------------
  shap_summary <- NULL
  cause_cols_present <- intersect(cause_lag_cols, colnames(X_full))
  if (length(cause_cols_present)) {
    dtest_full <- xgboost::xgb.DMatrix(Xte_full)
    shap <- predict(mod_full, dtest_full, predcontrib = TRUE)
    shap <- as.data.frame(shap)
    if (ncol(shap) > 0) {
      shap <- shap[, -ncol(shap), drop = FALSE]  # drop BIAS term
      m.shap <- colMeans(shap[, cause_cols_present, drop = FALSE], na.rm = TRUE)
      # tidy summary
      if (requireNamespace("stringr", quietly = TRUE)) {
        lags <- suppressWarnings(as.integer(stringr::str_extract(names(m.shap), "(?<=_L)\\d+")))
      } else {
        lags <- suppressWarnings(as.integer(sub(".*_L", "", names(m.shap))))
      }
      shap_summary <- data.frame(
        feature = names(m.shap),
        lag = lags,
        mean_shap = as.numeric(m.shap),
        stringsAsFactors = FALSE
      )
      shap_summary <- shap_summary[order(shap_summary$lag), , drop = FALSE]
    }
  }

  future::plan("sequential")

  list(
    improvement   = improvement,
    cw_p_value    = cw$p.value,
    cw_stat       = cw$statistic,
    cw_delta      = cw$mean_d,     # avg adjusted MSPE diff (>0 favors full)
    rmse_full     = rmse_full,
    rmse_reduced  = rmse_red,
    shap_lag_summary = shap_summary,
    n_oos         = length(y_oos)
  )
}

# scp /home/femeunier/Documents/projects/CausalAI/R/ml_granger_xgb0.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

