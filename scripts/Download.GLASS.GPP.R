# # Step 1
#
# rm(list = ls())
#
# library(rvest)
# library(xml2)
# library(httr)
# library(urltools)
# library(fs)
# library(parallel)
#
# # ------------------ CONFIG ------------------
# base_url  <- "http://www.glass.umd.edu/GPP/AVHRR/"
# dest_dir  <- "/data/gent/vo/000/gvo00074/felicien/GPP_data/GLASS"   # <- change if you like
# pattern   <- "\\.hdf$"                  # only .hdf files
# n_cores   <- max(1, detectCores() - 1)  # parallel downloads
# user_agent_str <- "R (bulk-downloader; contact: you@example.com)"
# # -------------------------------------------
#
# # Ensure trailing slash
# ensure_trailing_slash <- function(u) {
#   if (!grepl("/$", u)) paste0(u, "/") else u
# }
# base_url <- ensure_trailing_slash(base_url)
#
# # Read and extract absolute hrefs from a directory page
# safe_read_html <- function(u) {
#   resp <- RETRY("GET", u, user_agent(user_agent_str), times = 3, pause_base = 2)
#   stop_for_status(resp)
#   read_html(content(resp, "raw"))
# }
#
# list_links <- function(u) {
#   doc <- safe_read_html(u)
#   hrefs <- html_elements(doc, "a") |> html_attr("href")
#   hrefs <- hrefs[!is.na(hrefs)]
#   # Build absolute URLs and keep same-domain only
#   abs_urls <- url_absolute(hrefs, u)
#   abs_urls[grepl("^https?://", abs_urls)]
# }
#
# # Distinguish subdirectories vs files from listing heuristics
# is_subdir <- function(link) {
#   # Many Apache-style listings denote directories with trailing slash
#   grepl("/$", link)
# }
#
# # Recursively crawl to collect file URLs
# crawl_hdf <- function(root_url, pattern = "\\.hdf$", max_depth = 20) {
#   seen_dirs <- character(0)
#   to_visit  <- c(root_url)
#   files     <- character(0)
#
#   depth <- 0
#   while (length(to_visit) > 0) {
#     depth <- depth + 1
#     if (depth > 10000) stop("Too many iterations; aborting to prevent infinite loop.")
#     current <- to_visit[1]
#     to_visit <- to_visit[-1]
#
#     # skip if visited
#     if (current %in% seen_dirs) next
#     seen_dirs <- c(seen_dirs, current)
#
#     # List links
#     links <- tryCatch(list_links(current), error = function(e) {
#       message("Failed to list: ", current, " (", conditionMessage(e), ")")
#       return(character(0))
#     })
#     if (!length(links)) next
#
#     # Separate subdirs and files
#     dirs  <- links[is_subdir(links)]
#     # Heuristic: ignore parent directory links
#     dirs  <- dirs[!grepl("\\.\\./?$", dirs)]
#
#     furls <- links[grepl(pattern, links, ignore.case = TRUE)]
#     files <- c(files, furls)
#
#     # Enqueue subdirs within the same root (avoid going outside the tree)
#     same_tree <- dirs[startsWith(dirs, root_url)]
#     to_visit  <- c(to_visit, same_tree)
#   }
#
#   unique(files)
# }
#
# # Map a remote file URL to a local path preserving the subfolder structure
# remote_to_local <- function(file_url, base_url, dest_dir) {
#   rel <- sub(paste0("^", gsub("([.?*+^$(){}|\\[\\]\\/\\\\])", "\\\\\\1", base_url)), "", file_url)
#   # Clean any leading slashes
#   rel <- sub("^/", "", rel)
#   path(dest_dir, rel)
# }
#
# # Download one file with retries; skip if exists and size > 0
# download_one <- function(file_url, base_url, dest_dir) {
#   local_path <- remote_to_local(file_url, base_url, dest_dir)
#   dir_create(path_dir(local_path))
#
#   if (file_exists(local_path) && file_info(local_path)$size > 0) {
#     return(paste("SKIP (exists):", local_path))
#   }
#
#   # Encode URL path segments safely (avoid double-encoding the scheme/host)
#   # url_encode_path only encodes the path/query; here we split and reassemble
#   # Simpler heuristic: leave as-is unless spaces present
#   u <- if (grepl("\\s", file_url)) {
#     # encode the path part
#     parsed <- url_parse(file_url)
#     encoded_path <- paste(vapply(strsplit(parsed$path, "/")[[1]], URLencode, "", reserved = TRUE), collapse = "/")
#     url_compose(list(
#       scheme = parsed$scheme,
#       domain = parsed$domain,
#       port   = parsed$port,
#       path   = encoded_path,
#       parameter = parsed$parameter,
#       fragment  = parsed$fragment
#     ))
#   } else {
#     file_url
#   }
#
#   tf <- paste0(local_path, ".part")
#   resp <- tryCatch(
#     {
#       RETRY("GET", u,
#             user_agent(user_agent_str),
#             write_disk(tf, overwrite = TRUE),
#             progress(), times = 5, pause_base = 2)
#     },
#     error = function(e) e
#   )
#
#   if (inherits(resp, "error")) {
#     if (file_exists(tf)) file_delete(tf)
#     return(paste("FAIL:", file_url, "->", conditionMessage(resp)))
#   }
#
#   if (http_error(resp)) {
#     if (file_exists(tf)) file_delete(tf)
#     return(paste("HTTP FAIL:", file_url, "status", status_code(resp)))
#   }
#
#   file_move(tf, local_path)
#   paste("OK  :", local_path)
# }
#
# # --------------- RUN ----------------
# dir_create(dest_dir)
# hdf_urls <- crawl_hdf(base_url, pattern = pattern)
# hdf_urls <- hdf_urls[!grepl("YEAR",hdf_urls)]
#
# for (cfile in hdf_urls){
#
#   if (file.exists(file.path(dest_dir,basename(cfile)))) next()
#   system2("wget",
#           c(cfile,
#             "-P",dest_dir))
# }

# ################################################################################
# Step 2
rm(list = ls())

library(dplyr)
library(lubridate)
library(raster)

dest_dir  <- "/data/gent/vo/000/gvo00074/felicien/GPP_data/GLASS"   # <- change if you like
# dest_dir <- "~/Downloads/"
files <- list.files(dest_dir,
                    pattern = "^GLASS.*.hdf$",
                    full.names = TRUE)

cDate <- sub("A","",unlist(lapply(strsplit(basename(files),"\\."),"[[",3)))

all.years <- as.numeric(substr(cDate,1,4))
cDoY <- as.numeric(substr(cDate,5,7))
all.dates <- as.Date(cDoY-1,origin = paste0(all.years,"/01/01"))
all.months <- month(all.dates)

all.df <- data.frame(year = all.years,
                     month = all.months)


df <- all.df %>%
  dplyr::select(year,month) %>%
  distinct()

e <- extent(-180,180,-25,25)

for (i in seq(384,nrow(df))){

  cyear <- df$year[i] ; cmonth <- df$month[i]

  print(paste0(cyear,"-",cmonth))

  pos <- which((all.df$year == cyear) & (all.df$month == cmonth))

  all.r <- raster::crop(raster::stack(files[pos]),e)
  cr <- raster::calc(all.r, fun = mean, na.rm = TRUE)/1000*
    ifelse(lubridate::leap_year(paste0(cyear,"/01/01")),
           366,365)

  writeRaster(cr,
              file.path(dest_dir,
                        paste0("GPP.GLASS.",cyear,".",sprintf("%02d",cmonth),".tif")),
              options=c('TFW=YES'),
              overwrite = TRUE)

}

# scp /home/femeunier/Documents/projects/CausalAI/scripts/Download.GLASS.GPP.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/




