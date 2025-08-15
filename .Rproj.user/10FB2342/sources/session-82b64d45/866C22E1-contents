rm(list = ls())

library(dplyr)

files <- list.files("/data/gent/vo/000/gvo00074/ED_common_data/met/GLDAS/daily",
                    "*.RDS",full.names = TRUE)

df.precip <- readRDS("./outputs/df.GLDAS.Tropics.RDS")
all.df <- data.frame()
for (ifile in seq(1,length(files))){

  print(ifile/length(files))

  cfile <- files[ifile]
  cyear <- as.numeric(strsplit(basename(cfile),"\\.")[[1]][4])

  cdf <- readRDS(cfile)

  all.df <- bind_rows(all.df,
                      cdf)

}

df.merged <- df.precip %>%
  left_join(all.df,
            by = c("lon","lat","year","month"))

saveRDS(df.merged,
        "./outputs/df.GLDAS.Tropics.climate.RDS")

# source("resample.all.climate.files.R")
# cp /data/gent/vo/000/gvo00074/felicien/R/outputs/df.GLDAS.Tropics.climate.rspld.RDS /data/gent/vo/000/gvo00074/ED_common_data/met/Precip.Tropic
# source("combine.all.Tropical.climate.R")
