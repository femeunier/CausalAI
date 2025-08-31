rm(list = ls())

library(dplyr)

dir <- "/data/gent/vo/000/gvo00074/ED_common_data/met/Precip.Tropics/CMIP6"
files <- list.files(dir,pattern = "*.RDS$",
                    full.names = TRUE)

models <- unlist(lapply(strsplit(tools::file_path_sans_ext(basename(files)),"\\."),
                        "[[",3))

target.lon <- 24.502498 ; target.lat <- 0.814446
df.all <- data.frame()
for (imodel in seq(1,length(models))){

  cmodel <- models[imodel]

  print(paste0(imodel,"/",length(models)))

  cdf <- readRDS(files[imodel]) %>%
    ungroup() %>%
    mutate(lon_lat = paste0(lon,"_",lat))

  coord <- cdf %>%
    dplyr::select(lon,lat) %>%
    distinct() %>%
    mutate(target.lon,
           target.lat)

  dist <- coord %>%
    mutate(dist = sqrt((lon - target.lon)**2 + (lat - target.lat)**2)) %>%
    arrange(dist) %>%
    slice_head(n = 9) %>%
    mutate(lon_lat = paste0(lon,"_",lat))

  cdf.select <- cdf %>%
    filter(lon_lat %in% dist[["lon_lat"]]) %>%
    filter(year >= 1980) %>%
    mutate(pr = pr*86400*365/12)

  df.all <- bind_rows(df.all,
                      cdf.select)

}

saveRDS(df.all,
        "./outputs/climate.CMIP6.YGB.rspld.RDS")

# scp /home/femeunier/Documents/projects/CausalAI/scripts/extract.YGB.climate.CMIP6.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
