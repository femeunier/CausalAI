
make_lags <- function(data, max_lag = 6) {
  out <- data
  for (v in names(data)) for (L in 1:max_lag) {
    out[[paste0(v, "_L", L)]] <- dplyr::lag(data[[v]], L)
  }
  tidyr::drop_na(out)
}
