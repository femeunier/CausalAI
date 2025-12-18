rm(list = ls())

e <- extent(-180,180,-25,25)


years <- 1980:2023

dest_dir <- "/data/gent/vo/000/gvo00074/ED_common_data/met/CRUJRA/SM/"
for (cyear in years){

  cfile <- file.path(dest_dir,paste0("anl_land.225_soilw.reg_tl319.",cyear,
                                     "01_",cyear,"12"))
  mybrick <- stack(cfile)


  crs(mybrick) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

  shifted <- raster::shift(mybrick, dx = -360)
  merged  <- merge(shifted, mybrick)
  merged  <- crop(merged, extent(-180, 180, -25.27053, 25.27053))

  top.sml <- merged[[seq(1,36,3)]]
  bottom.sml <- merged[[seq(3,36,3)]]

  for (cmonth in 1:12){

    print(paste0(cyear,"-",cmonth))
    dest.file <- file.path(dest_dir,paste0("JRA_SM_",
                                           cyear,"_",sprintf("%02d",cmonth),".tif"))
    writeRaster(top.sml[[cmonth]],
                dest.file, overwrite = TRUE,
                wopt = list(gdal = "COMPRESS=LZW", datatype = "FLT4S"))

  }
}


# scp /home/femeunier/Documents/projects/CausalAI/scripts/Format.JRA_SM.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

