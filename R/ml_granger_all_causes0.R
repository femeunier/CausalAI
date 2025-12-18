ml_granger_all_causes0 <- function(df, dfl, mod_red, train_id, target, lags = 6,
                                  initial = 200, horizon = 12, step = 6,
                                  bestTune, verbose = 0) {
  causes <- setdiff(colnames(df), target)
  res_list <- lapply(causes, function(cause) {
    print(cause)
    r <- ml_granger_xgb0(dfl, mod_red, train_id, target, cause, lags, initial, horizon, step, bestTune, verbose)
    list(cause = cause, r = r)
  })

  results <- purrr::map_dfr(res_list, function(z) {
    tibble(cause = z$cause, target = target,
           improvement = z$r$improvement, p_value = z$r$cw_p_value,
           rmse_full = z$r$rmse_full, rmse_reduced = z$r$rmse_reduced,
           cw_stat = z$r$cw_stat,
           n_oos = z$r$n_oos)

  })
  shap_lags <- purrr::map_dfr(res_list, function(z) {
    if (is.null(z$r$shap_lag_summary)) return(NULL)
    z$r$shap_lag_summary %>% mutate(cause = z$cause, target = target)
  })
  list(results = results, shap_lags = shap_lags)
}


