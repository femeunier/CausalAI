rm(list = ls())

library(dplyr)
library(TrENDY.analyses)
library(raster)

coord <- expand.grid(lon = seq(-179.75,179.75,0.5),
                     lat = seq(-30.25,30.25,0.5))
rast <- rasterFromXYZ(coord %>%
                 mutate(value = 1))

Dir <- "/data/gent/vo/000/gvo00074/ED_common_data/met/Precip.Tropics"

files <- list.files(Dir,
                    full.names = TRUE)
files <- files[!grepl("rspld",files)]
files <- files[grepl("GLDAS",files)]
overwrite = TRUE

for (cfile in files){

  creanal <- strsplit(basename(cfile),"\\.")[[1]][2]

  print(creanal)

  OPfile <- file.path(Dir,paste0("df.",creanal,".Tropics.climate.rspd.RDS"))

  if (!overwrite & file.exists(OPfile)){
    next()
  }

  A <- readRDS(cfile)

  colnames(A)[colnames(A) %in% c("pr","MAP","Pmm")] <- "pre"
  colnames(A)[colnames(A) %in% c("tmn","tmin","T2MMIN")] <- "tasmin"
  colnames(A)[colnames(A) %in% c("tmx","tmax","T2MMAX")] <- "tasmax"
  colnames(A)[colnames(A) %in% c("tmp","T2MMEAN")] <- "tas"

  colNames <- colnames(A)

  A.rspld <- TrENDY.analyses::resample.df.all.col(bigdf = A,
                                                  raster2resample = rast,
                                                  var.names = colNames[!(colNames %in% c("year","month","lat","lon"))])


  saveRDS(A.rspld,
          OPfile)
}

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Resample.all.vars.Tropics.climate.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
