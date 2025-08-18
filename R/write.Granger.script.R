write.Granger.script <- function(dir.name,
                                 file.name,
                                 config.location,
                                 cmodel,
                                 global.suffix = "",
                                 lat.min,lat.max,lon.min,lon.max){

  file <- file.path(dir.name,file.name)

  writeLines("rm(list = ls())",con = file)
  write("",file=file,append=TRUE)
  write("CausalAI::load.everything()",file=file,append=TRUE)
  write("",file=file,append=TRUE)

  write(paste0("config <- readRDS(\"",
               config.location,
               "\")"),file=file,append=TRUE)
  write("",file=file,append=TRUE)

  write(paste0("lat.min <- ",lat.min),
        file=file,append=TRUE)
  write(paste0("lat.max <- ",lat.max),
        file=file,append=TRUE)
  write(paste0("lon.min <- ",lon.min),
        file=file,append=TRUE)
  write(paste0("lon.max <- ",lon.max),
        file=file,append=TRUE)

  write("",file=file,append=TRUE)
  write(paste0("cmodel <- \"",cmodel,"\""),
        file=file,append=TRUE)
  write(paste0("config[[\"","cmodel","\"","]] <- cmodel"),
        file=file,append=TRUE)

  write("",file=file,append=TRUE)
  write(paste0("config[[\"","lat.min","\"","]] <- lat.min"),
        file=file,append=TRUE)
  write(paste0("config[[\"","lat.max","\"","]] <- lat.max"),
        file=file,append=TRUE)
  write(paste0("config[[\"","lon.min","\"","]] <- lon.min"),
        file=file,append=TRUE)
  write(paste0("config[[\"","lon.max","\"","]] <- lon.max"),
        file=file,append=TRUE)
  write("",file=file,append=TRUE)

  write(paste0("dest.dir <- paste0(\"",dir.name,"\")"),
        file=file,append=TRUE)
  write(paste0("config[[\"","dest.dir","\"","]] <- dest.dir"),
        file=file,append=TRUE)

  write("",file=file,append=TRUE)
  write(paste0("suffix <- paste0(cmodel,",
               "\"_lats\",",lat.min,",\"_\",",lat.max,
               ",\"_lons\",",lon.min,",\"_\",",lon.max,
               ",\"_\",","\"",global.suffix,,"\"",")"),
        file=file,append=TRUE)

  write(paste0("config.file <- file.path(dest.dir, paste0(\"config_\",suffix,\".RDS\"))"),
        file=file,append=TRUE)

  write("",file=file,append=TRUE)
  write("dir.create(dest.dir, showWarnings = FALSE)",file=file,append=TRUE)
  write("saveRDS(config,config.file)",file=file,append=TRUE)
  write("run.Granger(config.file)",file=file,append=TRUE)


}
