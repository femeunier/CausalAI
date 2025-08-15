rm(list = ls())

library(dplyr)
library(ncdf4)
library(reshape2)
library(ggplot2)
library(lubridate)
library(raster)

# https://figshare.com/articles/dataset/Long-term_1982-2018_global_gross_primary_production_dataset_based_on_NIRv/12981977?file=24736712

first <- TRUE
SF <- 0.001

dir <- "/home/femeunier/Downloads/NIR.GPP/"

all.df <- data.frame()
for (cyear in seq(1982,2018)){

  czipfile <- file.path(dir,paste0(cyear,".zip"))
  system2("unzip",
          c(czipfile,"-d",
            dir))

  for (cmonth in seq(1,12)){

    file <- file.path(dir,cyear,cfile <- paste0("NIRv.GPP.",cyear,sprintf("%02d",cmonth),".v1.nc"))

    print(cfile)

    if (!file.exists(file)){next()}

    nc <- nc_open(file)

    if (first){
      lats <- ncvar_get(nc,"latitude")
      lons <- ncvar_get(nc,"longitude")
      first <- FALSE
    }

    Ndays <- as.numeric(days_in_month(paste0(cyear,
                                             "/",
                                             sprintf("%02d",cmonth),
                                             "/01")))

    GPP <- melt(ncvar_get(nc,"GPP")) %>%
      rename(lat = Var1,
             lon = Var2) %>%
      mutate(lat = lats[lat],
             lon = lons[lon]) %>%

      mutate(value = case_when(value != -9999 ~ value* SF,
                               TRUE ~ NA)) # gC/m²/d


    craster <- raster::aggregate(raster(suppressWarnings(SpatialPixelsDataFrame(points = GPP[c("lon","lat")],
                                                                                data = GPP["value"],
                                                                                tolerance = 0.01))),
                                 10)

    cdf <- as.data.frame(craster,xy = TRUE) %>%
      rename(lat = y,
             lon = x) %>%
      mutate(year = cyear,
             month = cmonth) %>%
      filter(!is.na(value)) %>%
      filter(abs(lat) <= 30)

    all.df <- bind_rows(all.df,
                        cdf)

  }

  system2("rm",
          c("-rf",
            file.path(dir,cyear)))
}

saveRDS(all.df,
        "./outputs/NIR.GPP.RDS")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = cdf) +
  geom_tile(aes(x=lon,y = lat,
                fill = value),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey") +
  # geom_sf(data = Amazon.shp,fill = NA, color = "black") +
  # coord_sf(xlim = c(-85, -30), ylim = c(-25, 10), expand = FALSE) +
  # scale_fill_gradient2(limits = c(-10,5)*1,
  #                      oob = scales::squish,
  #                      midpoint = 0,
  #                      low = "darkred",mid = "grey",high = "darkgreen") +
  labs(x = "",y = "") +
  theme_bw()

all.df.sum <- all.df %>%
  mutate(basin = case_when(lon <= 0 ~ "Amazon",
                           lon <= 50 ~ "Congo",
                           TRUE ~ "Else")) %>%
  group_by(basin,year,month) %>%
  summarise(gpp.m = mean(value),
            .groups = "keep") %>%
  mutate(time = year + (month - 1/2)/12)

ggplot(data = all.df.sum %>%
         filter(basin != "Else"),
       aes(x = time,
           y = gpp.m,
           color = basin)) +
  geom_line() +
  stat_smooth(method = "lm",
              se = FALSE) +
  theme_bw()
