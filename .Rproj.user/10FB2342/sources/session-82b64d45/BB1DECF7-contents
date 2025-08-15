rm(list = ls())

###############################################################################
# Rscript files XGBoost conditional

models <- c("CABLE-POP","CLASSIC","CLM6.0",
            "E3SM","JSBACH","JULES","LPJ-GUESS",
            "LPJmL","LPX-Bern","VISIT")

input_file <- "/data/gent/vo/000/gvo00074/felicien/R/XGBoost.grid.R"
lines <- readLines(input_file)

for (cmodel in models){
  output_file <- paste0("/data/gent/vo/000/gvo00074/felicien/R/XGBoost.grid.",
                        cmodel,
                        ".R")
  clines <- lines
  clines[20] <- paste0("cmodel <- '",cmodel,"'")

  writeLines(clines, output_file)
}

################################################################################

input_file <- "/data/gent/vo/000/gvo00074/felicien/R/jobR_basic.pbs"
lines <- readLines(input_file)

for (cmodel in models){
  output_file <- paste0("/data/gent/vo/000/gvo00074/felicien/R/jobR_",
                        cmodel,".pbs")
  clines <- lines
  # Modify line 5 (change index as needed)
  clines[12] <- paste0("Rscript XGBoost.grid.",cmodel,".R")

  writeLines(clines, output_file)
}

# paste0("qsub jobR_",models,".pbs")

# Copy.paste.scripts.DGVMs.R

#############################################################################
# Rscript files Granger causality

rm(list = ls())

models <- c("CABLE-POP","CLASSIC","CLM6.0",
            "E3SM","JSBACH","JULES","LPJ-GUESS",
            "LPJmL","LPX-Bern","VISIT")

input_file <- "/data/gent/vo/000/gvo00074/felicien/R/Granger.grid.R"
lines <- readLines(input_file)

for (cmodel in models){
  output_file <- paste0("/data/gent/vo/000/gvo00074/felicien/R/Granger.grid.",
                        cmodel,
                        ".R")
  clines <- lines
  clines[23] <- paste0("cmodel <- '",cmodel,"'")

  writeLines(clines, output_file)
}

################################################################################

input_file <- "/data/gent/vo/000/gvo00074/felicien/R/jobR_basic.pbs"
lines <- readLines(input_file)

for (cmodel in models){
  output_file <- paste0("/data/gent/vo/000/gvo00074/felicien/R/jobR_",
                        cmodel,"_Granger.pbs")
  clines <- lines
  # Modify line 5 (change index as needed)
  clines[12] <- paste0("Rscript Granger.grid.",cmodel,".R")

  writeLines(clines, output_file)
}

# paste0("qsub jobR_",models,".pbs")




