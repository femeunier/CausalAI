write.Granger.script <- function(dir.name,
                                 file.name,
                                 config.location,
                                 coord.location,
                                 cmodel,
                                 suffix = ""){

  file <- file.path(dir.name,file.name)

  writeLines("rm(list = ls())",con = file)
  write("",file=file,append=TRUE)
  write("CausalAI::load.everything()",file=file,append=TRUE)
  write("",file=file,append=TRUE)

  write(paste0("config <- readRDS(\"",
               config.location,
               "\")"),file=file,append=TRUE)

  write(paste0("lons_lats <- readRDS(\"",
               coord.location,
               "\")"),file=file,append=TRUE)

  write(paste0("dest.dir <- paste0(\"",dir.name,"\")"),
        file=file,append=TRUE)

  write("",file=file,append=TRUE)
  write(paste0("config[[\"","lons_lats","\"","]] <- lons_lats"),
        file=file,append=TRUE)

  write(paste0("config.file <- file.path(dest.dir, paste0(\"config_\",suffix,\".RDS\"))"),
        file=file,append=TRUE)

  write("",file=file,append=TRUE)
  write("dir.create(dest.dir, showWarnings = FALSE)",file=file,append=TRUE)
  write("saveRDS(config,config.file)",file=file,append=TRUE)
  write("run.Granger(config.file)",file=file,append=TRUE)


}
