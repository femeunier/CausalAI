rm(list = ls())


library(lubridate)
library(raster)
library(dplyr)

years <- 1980:2023
months <- 1:12
days <- 1:31

dest_dir <- "/data/gent/vo/000/gvo00074/ED_common_data/met/ESACCI"

e <- extent(-180,180,-25,25)

for (cyear in years){
  for (cmonth in months){
    Ndays <- lubridate::days_in_month(paste0(cyear,
                                             "/",sprintf("%02d",cmonth),
                                             "/01"))

    print(paste0(cyear,"-",cmonth))

    for (cday in (1:Ndays)){

      cname <- paste0("ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-",cyear,sprintf("%02d",cmonth),
                      sprintf("%02d",cday),"000000-fv09.1.nc")
      clink <- paste0("https://dap.ceda.ac.uk/neodc/esacci/soil_moisture/data/daily_files/COMBINED/v09.1/",cyear,
                      "/",cname,"?download=1")

      dest.file <- file.path(dest_dir,cname)

      if (file.exists(dest.file)) next()

      system2("wget",
              c(clink,"-O",dest.file))

    }

    files <- list.files(dest_dir,
                        pattern=paste0("^ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-",
                                       cyear,sprintf("%02d",cmonth),
                                       ".*.nc$"),
                        full.names = TRUE)

    r <- stack(files,varname = "sm")
    mean.sml <- mean(r,na.rm = TRUE)

    writeRaster(crop(r,e),
                file.path(dest_dir,
                          paste0("ESACCI.SM_",cyear,".",sprintf("%02d",cmonth),".tif")),
                options=c('TFW=YES'),
                overwrite = TRUE)


  }
}


# scp /home/femeunier/Documents/projects/CausalAI/scripts/download.ESACCI_SM.R hpc:/data/gent/vo/000/gvo00074/felicien/R

