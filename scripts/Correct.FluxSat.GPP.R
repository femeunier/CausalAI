rm(list = ls())

library(ncdf4)
library(dplyr)
library(ggplot2)
library(reshape2)
library(raster)

all.files <- rev(read.table("allFluxSatLinks.txt",
                            header = FALSE)[[1]])

dest.dir <- "/data/gent/vo/000/gvo00074/felicien/GPP_data/FluxSat/"
e <- extent(-180,180,-25,25)

for (ifile in seq(150,length(all.files))){

  clink <- all.files[ifile]

  ncfile <- file.path(dest.dir,basename(clink))

  cDate <- strsplit(basename(ncfile),"_")[[1]][6]
  cyear <- as.numeric(stringr::str_sub(cDate,1,4))
  cmonth <- as.numeric(stringr::str_sub(cDate,5,6))

  print(paste0(basename(ncfile)," - ", cyear,"/",cmonth))

  op.file1 <- file.path(dest.dir,
                       paste0('GPP.FLUXSAT.',cyear,".",sprintf("%02d",cmonth),".tif"))
  op.file2 <- file.path(dest.dir,
                        paste0('count_GPP.FLUXSAT.',cyear,".",sprintf("%02d",cmonth),".tif"))

  if (file.exists(op.file1) & file.exists(op.file2)){
    system2("mv",
            c(op.file2,
              file.path(dest.dir,"tmp1")))
    system2("mv",
            c(paste0(tools::file_path_sans_ext(op.file2),".tfw"),
              file.path(dest.dir,"tmp2")))

    system2("mv",
            c(op.file1,op.file2))
    system2("mv",
            c(paste0(tools::file_path_sans_ext(op.file1),".tfw"),
              paste0(tools::file_path_sans_ext(op.file2),".tfw")))

    system2("mv",
            c(file.path(dest.dir,"tmp1"),
              op.file1))
    system2("mv",
            c(file.path(dest.dir,"tmp2"),
              paste0(tools::file_path_sans_ext(op.file1),".tfw")))

    next()

  }

  if (!file.exists(ncfile)){
    system2("wget",
            c(clink,"-P",dest.dir))
  }

  GPP  <- crop(brick(ncfile, varname = "GPP"), e)
  QUAL <- crop(brick(ncfile, varname = "BRDF_Quality"), extent(GPP))

  # Set fill value to NA without touching all cells explicitly
  NAvalue(GPP) <- -9999

  GPP_masked <- overlay(GPP, QUAL,
                        fun = function(g, q) { g[q > 3] <- NA; g })

  # 0: best_quality_100_percent_with_full_inversions
  # 1: good_quality_75_percent_or_more_with_best_full_inversions_and_90_percent_or_more_with_full_inversions
  # 2: relative_good_quality_75_percent_or_more_with_full_inversions
  # 3: mixed_75_percent_or_less_full_inversions_and_25_percent_or_less_fill_values
  # 4: all_magnitude_inversions_or_50_percent_or_less_fill_values
  # 5: 50_percent_or_more_fill_values
  # 6: suspect_GPP_clipped
  # 7: missing_data_filled_with_climatology
  # 8: missing_data_zero_filled_low_PAR

  GPP_mean <- calc(GPP_masked, fun = function(x) mean(x, na.rm = TRUE))
  GPP_count <- calc(GPP_masked, fun = function(x) sum(!is.na(x)))


  writeRaster(GPP_count,
              file.path(dest.dir,
                        paste0('count_GPP.FLUXSAT.',cyear,".",sprintf("%02d",cmonth),".tif")),
              options=c('TFW=YES'),
              overwrite = TRUE)

  writeRaster((GPP_mean/1000*
                 ifelse(lubridate::leap_year(paste0(cyear,"/01/01")),
                        366,365)),
              op.file1,
              options=c('TFW=YES'),
              overwrite = TRUE)

  if (file.exists(ncfile)) system2("rm",ncfile)
}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Correct.FluxSat.GPP.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
