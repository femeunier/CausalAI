rm(list = ls())

library(terra)

years <- 1980:2024
months <- c(12)

dir <- "/data/gent/vo/000/gvo00074/ED_common_data/met/Tropics"

for (cyear in years){
  for (cmonth in months){

    print(paste0(cyear,"-",cmonth))

    cfile <- file.path(dir,
                       paste0("ERA5_SWC_",cyear,"_",sprintf("%02d",cmonth),".GRIB"))

    cdest.file <- file.path(dir,
                            paste0("ERA5_SWC_",cyear,"_",sprintf("%02d",cmonth),".grid"))

    if (!file.exists(cfile) | file.exists(cdest.file)) next

    zip.file <- file.path(paste0(tools::file_path_sans_ext(cfile),
                                 ".zip"))
    system2("mv",
            c(cfile,zip.file))

    system2("unzip",zip.file)

    system2("mv",
            c(file.path("data.grib"),
              cdest.file))

    system2("rm",zip.file)

  }
}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/correct.zip.files.ERA5.R hpc:/data/gent/vo/000/gvo00074/felicien/R
