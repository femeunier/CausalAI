deseason_detrend <- function(df, year.min, year.max) {
  stopifnot(all(c("month","year","year_decimal","CO2") %in% names(df)))
  df <- df[order(df$year_decimal), ]

  # Fit trend on reference window
  ref <- subset(df, year >= year.min & year <= year.max)
  trend_fit <- lm(CO2 ~ year_decimal, data = ref)

  # Detrend entire series using trend from the reference
  df$trend_pred      <- predict(trend_fit, newdata = df)
  df$co2detrended   <- df$CO2 - df$trend_pred

  # Mean seasonal cycle estimated on the reference window (after detrending)
  ref$co2detrended  <- ref$CO2 - predict(trend_fit, newdata = ref)
  season_means <- tapply(ref$co2detrended, ref$month, mean, na.rm = TRUE)

  # Subtract seasonal cycle everywhere
  df$mean_season     <- season_means[as.character(df$month)]
  df$co2anomaly     <- df$co2detrended - df$mean_season

  df
}
