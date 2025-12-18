clark_west_test <- function(y, f0, f1, h = 1,
                            alternative = c("greater","less","two.sided")) {
  alternative <- match.arg(alternative)
  ok <- is.finite(y) & is.finite(f0) & is.finite(f1)
  y <- y[ok]; f0 <- f0[ok]; f1 <- f1[ok]
  n <- length(y); if (n < 10L) return(list(statistic=NA_real_, p.value=NA_real_,
                                           mean_d=NA_real_, se=NA_real_, n=n, h=h))
  e0 <- y - f0
  e1 <- y - f1
  d  <- e0^2 - (e1^2 - (f0 - f1)^2)          # CW adjusted loss diff
  d_bar <- mean(d)

  # HAC (Newey–West) with Bartlett weights, L = h-1
  L <- max(0L, as.integer(h) - 1L)
  dc <- d - d_bar
  gamma0 <- sum(dc * dc) / n
  s <- gamma0
  if (L >= 1L) {
    for (l in 1L:L) {
      w <- 1 - l / (L + 1)
      g <- sum(dc[(l+1):n] * dc[1:(n-l)]) / n
      s <- s + 2 * w * g
    }
  }
  se <- sqrt(s / n)
  t  <- d_bar / se
  p  <- switch(alternative,
               greater   = 1 - pnorm(t),
               less      = pnorm(t),
               two.sided = 2 * (1 - pnorm(abs(t))))
  list(statistic = as.numeric(t), p.value = as.numeric(p),
       mean_d = d_bar, se = se, n = n, h = h, alternative = alternative)
}
