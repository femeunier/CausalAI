rm(list = ls())

library(ncdf4)
library(dplyr)
library(ggplot2)
library(reshape2)
library(raster)

all.files <- read.table("allFluxSatLinks.txt",
                        header = FALSE)[[1]]

dest.dir <- "/data/gent/vo/000/gvo00074/felicien/GPP_data/FluxSat/"

for (ifile in seq(1,length(all.files))){
  clink <- all.files[ifile]

  ncfile <- file.path(dest.dir,basename(clink))

  cDate <- strsplit(basename(ncfile),"_")[[1]][6]
  cyear <- as.numeric(stringr::str_sub(cDate,1,4))
  cmonth <- as.numeric(stringr::str_sub(cDate,5,6))

  print(paste0(basename(ncfile)," - ", cyear,"/",cmonth))

  op.file <- paste0(dest.dir,"/GPP.pantropical.",cyear,".",cmonth,".RDS")

  if (file.exists(op.file)){

    GPP.month <- readRDS(op.file)

    if (nrow(GPP.month) == 0) next()

    cr <- rasterFromXYZ(GPP.month[,c("lon","lat","value")] %>%
                          group_by()) /1000*
      ifelse(lubridate::leap_year(paste0(cyear,"/01/01")),
             366,365)

    writeRaster(cr,
                file.path(dest.dir,
                          paste0('GPP.FLUXSAT.',cyear,".",sprintf("%02d",cmonth),".tif")),
                options=c('TFW=YES'),
                overwrite = TRUE)


  } else {
    system2("wget",
            c(clink,"-P",dest.dir))


    if (!file.exists(ncfile)){
      next()
    }
    nc <- nc_open(ncfile)

    all.lats <- round(ncvar_get(nc,"lat"),digits = 3)
    pos.lats <- which(abs(all.lats) <= 23.5)
    lats <- all.lats[pos.lats]

    all.lons <- round(ncvar_get(nc,"lon"),digits = 3)
    pos.lons <- which(all.lons >= -120 & all.lons <= 160)
    lons <- all.lons[pos.lons]

    times <- as.Date(ncvar_get(nc,"time"),"2000/01/01")

    GPP <- ncvar_get(nc,"GPP",
                     start = c(min(pos.lons),min(pos.lats),1),
                     count = c(length(pos.lons),length(pos.lats),-1))

    BRDF_Quality <- ncvar_get(nc,"BRDF_Quality",
                              start = c(min(pos.lons),min(pos.lats),1),
                              count = c(length(pos.lons),length(pos.lats),-1))

    GPP[BRDF_Quality > 3] <- NA

    # 0: best_quality_100_percent_with_full_inversions
    # 1: good_quality_75_percent_or_more_with_best_full_inversions_and_90_percent_or_more_with_full_inversions
    # 2: relative_good_quality_75_percent_or_more_with_full_inversions
    # 3: mixed_75_percent_or_less_full_inversions_and_25_percent_or_less_fill_values
    # 4: all_magnitude_inversions_or_50_percent_or_less_fill_values
    # 5: 50_percent_or_more_fill_values
    # 6: suspect_GPP_clipped
    # 7: missing_data_filled_with_climatology
    # 8: missing_data_zero_filled_low_PAR

    nc_close(nc)

    GPP.month <- melt(apply(GPP,c(1,2),mean),na.rm = TRUE) %>%
      rename(lon = Var1,
             lat = Var2) %>%
      mutate(lon = lons[lon],
             lat = lats[lat]) %>%
      filter(!is.na(value)) %>%
      mutate(year = cyear,
             month = cmonth)

    saveRDS(GPP.month,
            op.file)

    system2("rm",c(ncfile))
  }
}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Download.FluxSat.GPP.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
