tune_xgb_with_caret_all <- function(train,
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

  if (requireNamespace("future", quietly = TRUE)) {
    Ncores <- as.numeric(future::availableCores())
    future::plan("multisession", workers = Ncores)
  } else {
    Ncores <- 1
  }


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


  slices <- createTimeSlices(1:nrow(x),
                             initialWindow = initial,
                             horizon       = horizon,
                             fixedWindow   = TRUE,
                             skip          = skip)

  ctrl <- trainControl(
    method = "cv",                 # we'll provide indices manually
    index = slices$train,          # training slices
    indexOut = slices$test,        # testing slices
    summaryFunction = defaultSummary,
    classProbs = FALSE,
    savePredictions = "final",
    verboseIter = TRUE,
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
    nthread = Ncores)

  future::plan("sequential")

  fit  # data.frame with tuned params incl. nrounds
}
