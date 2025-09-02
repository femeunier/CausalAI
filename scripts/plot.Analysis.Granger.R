rm(list = ls())

library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(raster)


suffix <- "gppanomaly"

system2("rsync",
        c("-avz",
          paste0("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/All.QoF.Granger",
                 ifelse(suffix == "",
                        suffix,
                        paste0(".",suffix)),".RDS"),
          "./outputs/"))

df.QoF <- readRDS(paste0("./outputs/All.QoF.Granger",ifelse(suffix == "",
                                                            suffix,
                                                            paste0(".",suffix)),".RDS"))

models <- sort(unique(df.QoF$model))

# selected.model <- df.QoF %>%
#   group_by(model) %>%
#   summarise(N = n()) %>%
#   ungroup() %>%
#   filter(N == max(N)) %>%
#   pull(model)
#
# selected.model <- "E3SM"

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = df.QoF) +
  geom_raster(aes(x = lon, y = lat,
                  fill = mean.y)) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  scale_y_continuous(limits = c(-1,1)*23.5) +
  scale_x_continuous(limits = c(-120,160)) +
  facet_wrap(~ model) +
  theme_map()

ggplot(data = df.QoF) +
  geom_raster(aes(x = lon, y = lat,color = outcome,
                  fill = outcome),
              size = 0.1) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  scale_y_continuous(limits = c(-1,1)*25) +
  scale_x_continuous(limits = c(-120,160)) +
  facet_wrap(~ model) +
  theme(legend.position = "bottom")

ggplot(data = df.QoF) +
  geom_raster(aes(x = lon, y = lat,color = outcome,
                  fill = outcome),
              size = 0.1) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  scale_y_continuous(limits = c(-10,5)) +
  scale_x_continuous(limits = c(-15,60)) +
  facet_wrap(~ model) +
  theme(legend.position = "bottom")

df.QoF %>%
  filter(model == "FLUXSAT") %>%
  filter(grepl("bug",tolower(outcome))) %>%
  pull(lon_lat) %>%
  unique()

table(df.QoF$outcome)

df.QoF %>%
  filter(model == "FLUXSAT") %>%
  filter(outcome != "fine") %>%
  filter(lon>15,lon<25,
         lat > 0, lat < 5)

ggplot(data = df.QoF) +
  geom_boxplot(aes(x = model,
                   y = mean.y)) +
  theme_bw()

ggplot(data = df.QoF) +
  geom_raster(aes(x = lon, y = lat,
                  fill = Rsq)) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  scale_y_continuous(limits = c(-1,1)*23.5) +
  scale_x_continuous(limits = c(-120,160)) +
  facet_wrap(~ model) +
  theme_map()


ggplot(data = df.QoF) +
  geom_boxplot(aes(x = model, y = Rsq)) +
  theme_bw()

################################################################################

system2("rsync",
        c("-avz",
          paste0("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/All.test.Granger",ifelse(suffix == "",
                                                                                             suffix,
                                                                                             paste0(".",suffix)),".RDS"),
          "./outputs/"))

df.test <- readRDS(paste0("./outputs/All.test.Granger",ifelse(suffix == "",
                                                              suffix,
                                                              paste0(".",suffix)),".RDS"))

ggplot(data = df.test,
       aes(x = pred, y = obs)) +
  geom_hex(aes(fill = stat(log(count)))) +
  stat_smooth(aes(color = model),
              method = "lm", se = FALSE) +
  geom_abline(slope = 1, intercept = 0, color = "black",
              linetype = 2) +
  facet_wrap(~ model) +
  coord_equal() +
  theme_bw()

################################################################################

system2("rsync",
        c("-avz",
          paste0("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/All.SHAP.Granger",ifelse(suffix == "",
                                                                                             suffix,
                                                                                             paste0(".",suffix)),".RDS"),
          "./outputs/"))

df.SHAP <- readRDS(paste0("./outputs/All.SHAP.Granger",ifelse(suffix == "",
                                                              suffix,
                                                              paste0(".",suffix)),".RDS"))

df.SHAP.sum <- df.SHAP %>%
  group_by(model,lon_lat, lon,lat,cause,target) %>%
  summarise(SHAP.m = abs(sum(mean_shap,na.rm = TRUE)),
            .groups = "keep")

ggplot(data = df.SHAP %>%
         group_by(model) %>%
         filter(lon_lat == lon_lat[1])) +
  geom_line(aes(x = lag,
                y = mean_shap,
                color = cause)) +
  facet_wrap(~ model) +
  theme_bw()

df.SHAP.max <- df.SHAP.sum %>%
  group_by(model,lon_lat,lon,lat,target) %>%
  arrange(desc(SHAP.m)) %>%
  slice_head(n = 1)

ggplot(data = df.SHAP.max) +
  geom_raster(aes(x = lon, y = lat,
                  fill = cause),) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  facet_wrap(~ model) +
  scale_y_continuous(limits = c(-1,1)*23.5) +
  scale_x_continuous(limits = c(-120,160)) +
  theme_map() +
  theme(legend.position = "bottom")

df.SHAP.max %>%
  group_by(model) %>%
  summarise(N = length(lon_lat),
            .groups = "keep")

df.SHAP.max2 <- df.SHAP.sum %>%
  ungroup() %>%
  mutate(cause.cat = case_when(cause %in% c("tmin","tmp","tmax","vpd") ~ "Demand",
                               cause %in% c("pre","top.sml") ~ "Supply",
                               TRUE ~ cause)) %>%
  group_by(model,lon_lat,lon,lat,
           cause.cat,target) %>%
  summarise(SHAP.m = sum(SHAP.m,na.rm = TRUE),
            .groups = "keep") %>%
  group_by(model,lon_lat,lon,lat,target) %>%
  arrange(desc(SHAP.m)) %>%
  slice_head(n = 1)

ggplot(data = df.SHAP.max2) +
  geom_raster(aes(x = lon, y = lat,
                  fill = cause.cat),) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  facet_wrap(~ model) +
  scale_y_continuous(limits = c(-1,1)*23.5) +
  scale_x_continuous(limits = c(-120,160)) +
  theme_map() +
  theme(legend.position = "bottom")

################################################################################

system2("rsync",
        c("-avz",
          paste0("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/All.results.Granger",ifelse(suffix == "",
                                                                                                suffix,
                                                                                                paste0(".",suffix)),".RDS"),
          "./outputs/"))

All.results.Granger <- readRDS(paste0("./outputs/All.results.Granger",ifelse(suffix == "",
                                                                             suffix,
                                                                             paste0(".",suffix)),".RDS"))

ggplot(data = All.results.Granger) +
  geom_raster(aes(x = lon, y = lat,
                  fill = improvement)) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  facet_wrap(~ model) +
  scale_y_continuous(limits = c(-1,1)*23.5) +
  scale_x_continuous(limits = c(-120,160)) +
  theme_map() +
  theme(legend.position = "bottom")

All.results.Granger.signif <- All.results.Granger %>%
  mutate(signif = case_when(p_value < 0.05 ~ TRUE,
                            TRUE ~ FALSE)) %>%
  group_by(model,lon,lat,target) %>%
  summarise(signif = any(signif),
            .groups = "keep")

ggplot(data = All.results.Granger.signif) +
  geom_raster(aes(x = lon, y = lat,
                  fill = signif)) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  facet_wrap(~ model) +
  scale_y_continuous(limits = c(-1,1)*23.5) +
  scale_x_continuous(limits = c(-120,160)) +
  theme_map() +
  theme(legend.position = "bottom")

