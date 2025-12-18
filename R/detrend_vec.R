# fast OLS residuals without calling lm() for every cell
detrend_vec <- function(y, x) {
  ok <- is.finite(y) & is.finite(x)
  if (sum(ok) < 2L) return(rep(NA_real_, length(y)))  # not enough points
  x0 <- x[ok]; y0 <- y[ok]
  xm <- mean(x0); ym <- mean(y0)
  vx <- sum((x0 - xm)^2)
  if (vx <= .Machine$double.eps) return(y - ym)       # flat time axis → just de-mean
  b <- sum((x0 - xm) * (y0 - ym)) / vx                 # slope
  a <- ym - b * xm                                     # intercept
  y - (a + b * x)                                      # residuals = detrended series
}
