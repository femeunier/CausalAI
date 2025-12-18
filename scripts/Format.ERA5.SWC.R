rm(list = ls())

library(raster)

years <- (1980:2024)
months <- 1:12

input.dir <- "/data/gent/vo/000/gvo00074/ED_common_data/met/Tropics/"
dest.dir <- "./outputs/ERA5"

overwrite <- FALSE
for (cyear in years){
  for (cmonth in months){


    print(paste0(cyear,"-",cmonth))

    op.file <- file.path(dest.dir,paste0("top.sml_",
                              cyear,sprintf("%02d",cmonth),"_ERA5.tif"))

    if (file.exists(op.file) & !overwrite) next()

    B <- stack(file.path(input.dir,
                         paste0("ERA5_SWC_",cyear,"_",
                                sprintf("%02d",cmonth),".grid")))
    layers <- rep(1:4, length.out = nlayers(B))
    mean.sml <- stackApply(B, indices = layers, fun = mean)

    top.sml <- mean.sml[[1]]
    tot.sml <- (7*mean.sml[[1]]+21*mean.sml[[2]] +
                  72*mean.sml[[3]] + 189*mean.sml[[4]])/(7+21+72+189)


    writeRaster(top.sml,
                op.file,
                overwrite = TRUE,
                wopt = list(gdal = "COMPRESS=LZW", datatype = "FLT4S"))

    writeRaster(tot.sml,
                file.path(dest.dir,paste0("tot.sml_",
                                          cyear,sprintf("%02d",cmonth),"_ERA5.tif")),
                overwrite = TRUE,
                wopt = list(gdal = "COMPRESS=LZW", datatype = "FLT4S"))

  }
}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Format.ERA5.SWC.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/





