rm(list = ls())

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(tseries)
library(vars)
library(xgboost)
library(caret)
library(tidyverse)
library(shapr)
library(yardstick)
library(reshape2)
library(BigVAR)
library(Metrics)
library(forecast)
library(stringr)
library(purrr)

# system2("rsync",
#         c("-avz",
#           "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/Example.MSL.RDS",
#           "./outputs/"))

# # Manaus
# target.lon <- -60.2
# target.lat <- -2.5

CO2 <- read.table("/home/femeunier/Documents/projects/Congo.vs.Amazon/data/global_co2_ann_1700_2024.txt") %>%
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


climate <- readRDS("/home/femeunier/Documents/data/monthly.climate.pantropical.CRUJRA.RDS") %>%
  filter(lon == -60.25,
         lat  == -2.25) %>%
  mutate(lat = round(lat,digits = 2),
         lon = round(lon,digits = 2)) %>%
  left_join(monthly_df,
            by = c("year","month")) %>%
  ungroup()

msl.ts <- readRDS("./outputs/Example.MSL.RDS") %>%
  pivot_wider(names_from = soil.layer,
              values_from = value,
              names_prefix = "soil.layer.") %>%
  mutate(year = year(time),
         month = month(time)) %>%
  dplyr::select(lon,lat,year,month,
                starts_with("soil.layer.")) %>%
  mutate(lat = round(lat,digits = 2),
         lon = round(lon,digits = 2)) %>%
  mutate(lon = case_when(lon > 180 ~ lon - 360,
                         TRUE ~ lon))

CC <- readRDS("~/Downloads/Trendy.CABLE-POP.S2.CC.pantropical.v13.RDS") %>%
  filter(lon == -60.5,
         lat  == -2.5) %>%
  dplyr::select(lon,lat,year,month,gpp) %>%
  mutate(lat = round(lat,digits = 2),
         lon = round(lon,digits = 2))

all <- climate %>%
  mutate(lat = -2.5,
         lon = -60.5) %>%
  filter(year > 1991) %>%
  left_join(msl.ts %>%
              mutate(lat = -2.5,
                     lon = -60.5),
            by = c("lon","lat","year","month")) %>%
  left_join(CC %>%
              mutate(lat = -2.5,
                     lon = -60.5),
            by = c("lon","lat","year","month"))

df <- all %>%
  dplyr::select(-any_of(c("lon","lat","year","month"))) %>%
  dplyr::select(pre,tmp,tmin,tmax,
                dswrf,spfh,VPD,
                CO2,
                soil.layer.1,gpp) %>%
  mutate(gpp = as.numeric(gpp)*86400*365)

plot(df$CO2, type = "l")
plot(df$gpp,type = "l")
plot(df$tmin, type = "l")
plot(df$CO2,df$gpp, type = "p")

################################################################

scale_z <- function(df, mu, sdv) {
  as.data.frame(mapply(function(z, m, s) (z - m) / s, df, mu, sdv))
}

climate_vars <- c("tmp", "tmin", "tmax",
                  "dswrf", "CO2",
                  "pre", "VPD", "soil.layer.1")
x_var <- climate_vars
y_var <- "gpp"

mu <- sapply(df, mean) ; mu[y_var] <- 0
sdv <- sapply(df, sd) ; sdv[y_var] <- 1

df <- scale_z(df, mu, sdv)

################################################################
# Granger causality
VARselect(df, lag.max = 12)

results <- lapply(climate_vars, function(var) {
  df_subset <- df[, c("gpp", var)]

  if (anyNA(df_subset)) return(NA)
  tryCatch({
    var_model <- VAR(df_subset, p = 6, type = "const")  # Use p = 1, or use VARselect to determine optimal lag
    granger_test <- causality(var_model, cause = var)$Granger
    data.frame(
      variable = var,
      F = granger_test$statistic,
      p_value = granger_test$p.value
    )
  }, error = function(e) {
    data.frame(variable = var, F = NA, p_value = NA)
  })
})

granger_results <- do.call(rbind, results)
granger_results <- granger_results[order(desc(granger_results$F)), ]
print(granger_results)

#######################################################################
# Granger causality 2


make_lags <- function(data, max_lag = 6) {
  out <- data
  for (v in names(data)) for (L in 1:max_lag) {
    out[[paste0(v, "_L", L)]] <- dplyr::lag(data[[v]], L)
  }
  tidyr::drop_na(out)
}

# ---- 1) TUNE once with caret on FULL feature set (target ~ all lags) ----
tune_xgb_with_caret <- function(train,
                                y,
                                target, lags = 6,
                                initial = 200, horizon = 12, skip = 6,
                                grid = expand.grid(
                                  nrounds = c(200, 400, 800),
                                  max_depth = c(3, 6, 9),
                                  eta = c(0.03, 0.1),
                                  gamma = c(0, 0.1),
                                  colsample_bytree = c(0.8, 1.0),
                                  min_child_weight = c(1),
                                  subsample = c(0.8, 1.0)
                                )) {
  # dfl <- make_lags(df, max_lag = lags)
  # y <- dfl[[target]]
  train <- train[, grep("_L[0-9]+$", colnames(train)), drop = FALSE]  # only lagged features

  dfx <- if (is.matrix(train)) as.data.frame(train) else train

  # 1) convert logical predictors → integer (0/1)
  if (any(vapply(dfx, is.logical, logical(1)))) {
    dfx <- transform(dfx, !!!lapply(dfx[vapply(dfx, is.logical, logical(1))], as.integer))
  }

  # 2) coerce everything to numeric matrix; set storage mode to double
  x <- data.matrix(dfx)
  storage.mode(x) <- "double"

  # 3) y must be pure numeric *vector* (not logical/factor/matrix)
  y <- as.numeric(y)
  stopifnot(is.numeric(y), is.vector(y), length(y) == nrow(x))

  # 4) clean impossible values
  if (any(!is.finite(x))) x[!is.finite(x)] <- NA_real_
  if (any(!is.finite(y))) stop("y contains non-finite values")

  # 5) drop rows with any NA in x or y (or impute upstream)
  keep <- complete.cases(x) & is.finite(y)
  x <- x[keep, , drop = FALSE]
  y <- y[keep]


  ctrl <- trainControl(
    method = "timeslice",
    initialWindow = initial,
    horizon = horizon,
    fixedWindow = TRUE,
    summaryFunction = defaultSummary,  # <-- regression metrics (RMSE/RSquared/MAE)
    classProbs = FALSE,                # <-- must be FALSE for regression
    savePredictions = "final",
    verboseIter = TRUE,
    skip = skip,
    allowParallel = TRUE
  )

  fit <- train(
    x = as.data.frame(x),
    y = y,
    method = "xgbTree",
    trControl = ctrl,
    tuneGrid = grid,
    metric = "RMSE",
    verbosity = 1,
    nthread = 16)

  fit  # data.frame with tuned params incl. nrounds
}

# ---- 2) ML-Granger for one (cause -> target), using tuned params ----
ml_granger_xgb <- function(dfl, target, cause, lags = 12,
                           initial = 200, horizon = 12, step = 6,
                           bestTune, verbose = 0) {
  y <- dfl[[target]]

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
      mabs <- colMeans(abs(shap[, cause_cols_present, drop = FALSE]))
      shap_accum <- rbind(shap_accum, mabs)
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
      mean_abs_shap = as.numeric(shap_mean)
    ) %>% arrange(lag)
  }

  list(improvement = improvement, p_value = pval, dm_stat = stat,
       rmse_full = rmse_full, rmse_reduced = rmse_red,
       shap_lag_summary = shap_summary)
}

# ---- 3) Loop over all causes for your single target ----
ml_granger_all_causes <- function(df, dfl, target, lags = 6,
                                  initial = 200, horizon = 12, step = 6,
                                  bestTune, verbose = 0) {
  causes <- setdiff(colnames(df), target)
  res_list <- lapply(causes, function(cause) {
    print(cause)
    r <- ml_granger_xgb(dfl, target, cause, lags, initial, horizon, step, bestTune, verbose)
    list(cause = cause, r = r)
  })

  results <- purrr::map_dfr(res_list, function(z) {
    tibble(cause = z$cause, target = target,
           improvement = z$r$improvement, p_value = z$r$p_value,
           rmse_full = z$r$rmse_full, rmse_reduced = z$r$rmse_reduced,
           dm_stat = z$r$dm_stat)
  })
  shap_lags <- purrr::map_dfr(res_list, function(z) {
    if (is.null(z$r$shap_lag_summary)) return(NULL)
    z$r$shap_lag_summary %>% mutate(cause = z$cause, target = target)
  })
  list(results = results, shap_lags = shap_lags)
}


# ===== 4) RUN (example) =====
# df <- your_data_frame  # numeric, time-ordered; colnames like: c("Y","X1",...,"X10")

lags <- 12 ; initial <- 200
horizon <- 12; skip <- 6; step <- 6

dfl <- make_lags(df, max_lag = lags) %>%
  na.omit()

smp_size <- floor(0.8 * nrow(dfl))
train_ind <- sample(seq_len(nrow(df)),
                    size = smp_size)

df.train <- df[train_ind,c(x_var,y_var)]
dfl.train <- as.matrix(dfl[train_ind,
                           setdiff(colnames(dfl), y_var)])
y.train <- as.matrix(dfl[train_ind, y_var])

df.test <- df[-train_ind,c(x_var,y_var)]
dfl.test <- as.matrix(dfl[-train_ind, setdiff(colnames(dfl), y_var)])
y.test <- as.matrix(dfl[-train_ind, y_var])

fit <- tryCatch(tune_xgb_with_caret(dfl.train,
                                    y.train,
                                    grid = expand.grid(
                                      nrounds = c(200),
                                      max_depth = c(5),
                                      eta = c(0.02),
                                      gamma = c(0, 0.1),
                                      colsample_bytree = c(1.0),
                                      min_child_weight = c(1),
                                      subsample = c( 1.0)
                                    ),
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
RSQ <- rsq_vec(as.numeric(y.pred), y.test)
rBias <- mean(100*(y.test - y.pred)/y.test,
              na.rm = T)

run <- tryCatch(ml_granger_all_causes(df, dfl, target = "gpp",
                                      lags = lags, initial = initial, horizon = horizon, step = step,
                                      bestTune = bestTune),
                error = function(e) NULL)


if (is.null(run)) next()

shap_df <- run$shap_lags %>%
  as.data.frame()

all.SHAP <- bind_rows(all.SHAP,
                      shap_df %>%
                        mutate(model = cmodel,
                               lon_lat = clon.lat))

# ===== 5) Plots =====
# (a) Improvement heatmap (single-row)
ggplot(run$results, aes(x = cause, y = target, fill = improvement)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.3f", improvement)), size = 3) +
  scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0) +
  labs(title = "ML-Granger (XGBoost tuned via caret): RMSE_reduced - RMSE_full",
       x = "Cause", y = "", fill = "Improvement") +
  theme_minimal(base_size = 12)

# (b) Evidence heatmap: -log10(p) from DM test
res_p <- run$results %>%
  mutate(p_clip = pmax(p_value, .Machine$double.eps),
         nlog10p = -log10(p_clip))
ggplot(res_p, aes(x = cause, y = target, fill = p_value)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", p_value)), size = 3) +
  labs(title = "Diebold–Mariano evidence (p_value)",
       x = "Cause", y = "", fill = "p_value") +
  theme_minimal(base_size = 12)

# (c) SHAP by lag for the top improved cause
top_cause <- run$results %>%
  arrange(desc(improvement)) %>%
  slice_head(n = 3) %>%
  pull(cause)

run$shap_lags %>%
  filter(cause %in% top_cause) %>%
  ggplot(aes(x = lag, y = mean_abs_shap, fill = cause)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  labs(title = paste0("Which lags of X drive Y? (mean |SHAP|)"),
       x = "Lag", y = "Mean |SHAP|") +
  theme_minimal(base_size = 12)

stop()

# ######################################################################
# df.scaled <- df %>%
#   mutate(across(everything(), ~ scale(.)[,1]))
#
# Y_and_X <- as.matrix(df.scaled)
# var_names <- colnames(Y_and_X)
# Nlags <- 6
#
# model_spec <- constructModel(Y_and_X, p = Nlags, struct = "Lag",
#                              gran = c(10, 10), verbose = FALSE)
# model_fit <- cv.BigVAR(model_spec)
# coeffs <- model_fit@betaPred
#
# rownames(coeffs) <- var_names
# lagged_names <- unlist(lapply(1:Nlags, function(lag) paste0(var_names, "_L", lag)))
# colnames(coeffs) <- c("Intercept", lagged_names)
#
# beta_Y <- coeffs["gpp",]
# print(sort(abs(beta_Y)))
#
# ################################################################
# XGBoost


grid <- expand.grid(
  nrounds = c(300, 500, 800, 1200),
  eta = c(0.02, 0.05, 0.1, 0.5),
  max_depth = c(4, 6, 9, 12),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1)

control <- trainControl(method = "cv", number = 5)

smp_size <- floor(0.8 * nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

x_train <- as.matrix(df[train_ind, x_var])
y_train <- df[train_ind, ] %>% pull(y_var)

x_test <- as.matrix(df[-train_ind, x_var])
y_test <- df[-train_ind, ] %>% pull(y_var)

model <- train(
  x = x_train,
  y = y_train,
  method = "xgbTree",
  trControl = control,
  tuneGrid = grid,
  metric = "RMSE",
  verbosity = 0,
  nthread = 16)

pred_y <- predict(model, x_test)
RMSE <- caret::RMSE(pred_y, y_test)
RSQ <- rsq_vec(as.numeric(pred_y), y_test)
rBias <- (mean(y_test, na.rm = T)-mean(pred_y, na.rm = T))*100/mean(y_test, na.rm = T)

plot(pred_y,y_test,type = "p")
abline(a = 0, b = 1, col = "red")
print(paste(RMSE,"-",RSQ,"-",rBias))

if (requireNamespace("future", quietly = TRUE)) {
  future::plan("multisession", workers = 2)
}

if (requireNamespace("progressr", quietly = TRUE)) {
  progressr::handlers(global = TRUE)
}

group_ts <- list(
  S1 = c("tmp","tmin","tmax","VPD"),
  S2 = c("dswrf"),
  S3 = c("pre","soil.layer.1"),
  S4 = c("CO2"))

explainer <- explain(model$finalModel,x_test,x_train,
                     approach = "timeseries",
                     phi0 = mean(y_train),
                     group = group_ts)

shap_df <- explainer$shapley_values_est %>%
  as.data.frame() %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(-row_id, names_to = "feature", values_to = "shap_value") %>%
  filter(!(feature %in% c("explain_id","none")))


ggplot(shap_df,
       aes(x = shap_value, y = feature)) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  theme_minimal() +
  geom_vline(xintercept = 0) +
  labs(x = "SHAP value", y = "Feature",
       title = "mean SHAP value")

ggplot(shap_df,
       aes(x = shap_value, y = feature)) +
  geom_violin(fill = NA, alpha = 0.5) +
  geom_boxplot(width = 0.2, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  theme_minimal() +
  geom_vline(xintercept = 0,
             linetype = 2) +
  labs(x = "SHAP value", y = "Feature", title = "SHAP value distribution")

shap_df %>%
  group_by(feature) %>%
  summarise(mean_abs_shap = mean(abs(shap_value),na.rm = TRUE)) %>%
  arrange(desc(mean_abs_shap)) %>%
  ggplot(aes(x = reorder(feature, mean_abs_shap), y = mean_abs_shap)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(x = "Feature", y = "Mean |SHAP|", title = "Feature importance (SHAP)") +
  theme_minimal()



