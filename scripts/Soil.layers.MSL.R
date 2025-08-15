rm(list = ls())

library(dplyr)

layer.depths <-
  bind_rows(
    data.frame(z = c(0.022,0.058,0.154,0.409,1.085,6.872),
               model = "CABLE-POP")
  )
