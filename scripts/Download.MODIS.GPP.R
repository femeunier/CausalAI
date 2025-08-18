# Step 1

# rm(list = ls())
#
# # Load required libraries
# library(rgee)
# library(sf)
# library(lubridate)
# library(raster)
#
# # reticulate::py_install("earthengine-api")
#
# # Initialize Earth Engine
# # ee_Initialize()
# reticulate::py_run_file('auth.py')
#
# # SETTINGS
# collection_id <- "MODIS/061/MOD17A2HGF" # gap-filled GPP
#
# # Dates in R
# start_date <- as.Date("2000-02-01")
# end_date   <- as.Date("2024-12-01")
# months <- seq(start_date, end_date, by = "1 month")
#
# # Geometry
# region <- ee$Geometry$Rectangle(c(-180, -23.5, 180, 23.5), "EPSG:4326", FALSE)
#
# # Prep MODIS collection (scaled once)
# col <- ee$ImageCollection(collection_id)$select("Gpp")$
#   map(ee_utils_pyfunc(function(img) {
#     img$multiply(0.0001)$rename("GPP")$
#       copyProperties(img, img$propertyNames())
#   }))
#
# tasks <- vector("list", length(months))  # <- store tasks here
#
# for (imonth in seq(1,length(months))) {
#
#   cmonth <- months[imonth]
#   y <- year(cmonth)
#   m <- sprintf("%02d", month(cmonth))
#   d_ee <- ee$Date(format(cmonth, "%Y-%m-%d"))
#
#   print(paste0(y,"-",m))
#
#   mcol <- col$filterDate(d_ee, d_ee$advance(1, "month"))
#   mimg <- ee$Image(
#     ee$Algorithms$If(
#       mcol$size()$gt(0),
#       mcol$mean()$rename("GPP_monthly_mean"),
#       ee$Image$constant(0)$updateMask(ee$Image$constant(0))$rename("GPP_monthly_mean")
#     )
#   )
#
#   task <- ee_image_to_drive(
#     image = mimg,
#     description    = paste0("MODIS_GPP_mean_0p05deg_", y, "_", m),
#     folder         = "GEE_Exports",
#     fileNamePrefix = paste0("MODIS_GPP_mean_0p05deg_", y, "_", m),
#     region         = region,
#     crs            = "EPSG:4326",
#     fileFormat     = "GeoTIFF",
#     scale          = 5566,
#     maxPixels      = 1e13
#   )
#
#   tasks[[imonth]] <- task
# }
#
# lapply(tasks, function(t) {
#   st <- t$status()
#   st_state <- st[["state"]]
#   if (is.null(st_state) || st_state == "UNSUBMITTED") t$start()
# })
#
# ee_monitoring(task = tasks, max_attempts = 999999)
#
# # scp /home/femeunier/Documents/projects/CausalAI/scripts/download.MODIS.GPP.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

# Step 2

rm(list = ls())

library(raster)

dir <- "/data/gent/vo/000/gvo00074/felicien/GPP_data/MODIS_GPP/"
files <- list.files(dir, pattern = "\\.tif$", full.names = FALSE)

years <- as.numeric(lapply(strsplit(files,"\\_"),"[[",5))
months <- as.numeric(lapply(strsplit(files,"\\_"),"[[",6))


for (ifile in seq(1,length(files))){

  print(paste0(ifile,"/",length(files)))

  r <- raster(file.path(dir,files[length(files)]))
  writeRaster(r/8* ifelse(lubridate::leap_year(paste0(years[ifile],"/01/01")),
                          366,365),
              file.path(dir,
                        paste0('GPP.MODIS.',years[ifile],
                               ".",sprintf("%02d",months[ifile]),".tif")),
              options=c('TFW=YES'),
              overwrite = TRUE)
}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Download.MODIS.GPP.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/


