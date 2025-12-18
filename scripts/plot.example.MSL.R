rm(list = ls())

library(ggplot2)
library(lubridate)

system2("rsync",
        c("-avz",
          "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/Example.MSL.RDS",
          "./outputs/"))

msl.ts <- readRDS("./outputs/Example.MSL.RDS") %>%
  ungroup() %>%
  mutate(month = month(time),
         year = year(time))

ggplot(data = msl.ts %>%
         filter(lon == lon[1], lat == lat[1])) +
  geom_line(aes(x = year + (month - 1/2)/12, y = value)) +
  facet_wrap(~ as.factor(soil.layer), scales = "free") +
  theme_bw()


msl.ts.SC <- msl.ts %>%
  mutate(month = month(time)) %>%
  group_by(lon,lat,soil.layer,month) %>%
  summarise(msl.m = mean(value,na.rm = TRUE),
            .groups = "keep") %>%
  group_by(lon,lat,soil.layer) %>%
  mutate(msl.m.norm = (msl.m - min(msl.m))/(max(msl.m) - min(msl.m)))


ggplot(data = msl.ts.SC %>%
         filter(lon == lon[1], lat == lat[1])) +
  geom_line(aes(x = month, y = msl.m.norm,
            color = as.factor(soil.layer))) +
  theme_bw()


