# https://figshare.com/articles/dataset/Improved_estimate_of_global_gross_primary_production_for_reproducing_its_long-term_variation_1982-2017/8942336/3

library(ncdf4)
library(stringr)
library(lubridate)
library(raster)
library(dplyr)

dir <- '/data/gent/vo/000/gvo00074/felicien/GPP_data/Zheng'
files <- list.files(dir,pattern = "*.hdf",recursive = TRUE,full.names = TRUE)

year <- as.integer(str_extract(files, "(?<=\\.)\\d{4}(?=\\d{3}\\.)"))
doy  <- as.integer(str_extract(files, "(?<=\\d{4})\\d{3}(?=\\.hdf)"))

# Build date
dates <- as.Date(doy - 1, origin = paste0(year, "-01-01"))

years  <- year(dates)
months <- month(dates)

all.df <- data.frame(file = files,
                     date = dates,
                     year = years,
                     month = months)

df <- all.df %>%
  dplyr::select(year,month) %>%
  distinct()

e <- extent(-180,180,-25,25)

for (i in seq(1,nrow(df))){
  cyear <- df$year[i] ; cmonth <- df$month[i]

  print(paste0(cyear,"-",cmonth))

  pos <- which((years == cyear) & (months == cmonth))

  r <- crop(stack(files[pos]),e)
  r[r == 65535] <- NA

  writeRaster(mean(r,na.rm = TRUE)/1000* ifelse(lubridate::leap_year(paste0(cyear,"/01/01")),
                                            366,365)*0.01,
              file.path(dir,
                        paste0('GPP.Zheng.',cyear,
                               ".",sprintf("%02d",cmonth),".tif")),
              options=c('TFW=YES'),
              overwrite = TRUE)
}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Download.Zheng.GPP.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/


