rm(list = ls())

library(stringr)
library(dplyr)
library(cowplot)
library(tidyr)
library(ggplot2)
library(raster)
library(sf)
library(scales)

system2("rsync",
        c("-avz",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/All.Transitions.timing.RDS",
          "./outputs/"))

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
rainforests <- read_sf("./data/Rainforests_CMIP6.shp")

GridArea <- readRDS("./outputs/GridArea.RDS")
LC <- readRDS("./data/LC_Congo.RDS") %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2))

All.Transitions <- readRDS("./outputs/All.Transitions.timing.RDS") %>%
  mutate(LC.pred_Long_future = as.numeric(LC.pred_Long_future),
         LC.pred_Mid_future = as.numeric(LC.pred_Mid_future),
         LC.pred_Near_future = as.numeric(LC.pred_Near_future),
         LC.pred_historical = as.numeric(LC.pred_historical),) %>%
  dplyr::select(-Delta_MCWD) %>%
  pivot_longer(cols = -c(scenario,model,lon,lat,LC,change,seasonality),
               names_to = "variable",
               values_to = "value") %>%
  mutate(period = str_replace(variable,
                              "MCWD_|MAP_|LC.pred_",""),
         var = str_replace(variable,
                              "_Long_future|_Mid_future|_Near_future|_historical","")) %>%
  dplyr::select(-variable) %>%
  distinct()

All.Transitions.ref <- All.Transitions %>%
  left_join(All.Transitions %>%
              filter(period == "historical") %>%
              dplyr::select(-c(period)) %>%
              rename(value.ref = value),
            by = c("model","LC","lon","lat","scenario",
                   "change","seasonality","var"))


All.Transitions.ref.wide <- All.Transitions.ref %>%
  pivot_wider(names_from = var,
              values_from = c(value,value.ref))

GW <- readRDS("./outputs/Global.warming.MEM.sel.RDS")
GW <- readRDS("./outputs/Global.warming.MEM.RDS")

df.temp <- GW %>%
  mutate(period = case_when(year %in% 1850:1879 ~ "pi",
                            year %in% 1981:2010 ~ "historical",
                            year %in% 2011:2040 ~ "Near_future",
                            year %in% 2041:2070 ~ "Mid_future",
                            year %in% 2071:2100 ~ "Long_future",
                            TRUE ~ NA_character_)) %>%
  filter(!is.na(period)) %>%
  group_by(model,scenario,period) %>%
  summarise(tas.m = mean(tas),
            .groups = "keep") %>%
  ungroup()

df.GW <- df.temp %>%
  filter(
    (scenario != "historical") |
      (scenario == "historical" & period == "historical")) %>%
  left_join(df.temp %>%
              filter(period == "pi") %>%
              dplyr::select(-c(scenario,period)) %>%
              rename(tas.m.ref = tas.m),
            by = c("model")) %>%
  mutate(gw = tas.m - tas.m.ref)

df.GW.hist <- df.GW %>%
  filter(period == "historical")
scenarios <- unique(df.GW %>%
                      filter(scenario != "historical") %>%
                      pull(scenario))
df.GW <- df.GW %>%
  filter(period != "historical")

for (cscenario in scenarios){
  df.GW <- bind_rows(df.GW,
                     df.GW.hist %>%
                       mutate(scenario = cscenario))
}

saveRDS(df.GW,
        "./outputs/df.Global.warming_periods.RDS")


ggplot(data = df.GW) +
  geom_boxplot(aes(x = scenario, fill = scenario,
                   y = gw)) +
  facet_wrap(~ period) +
  theme_bw()

cAll.Transitions <- All.Transitions.ref.wide %>%
  filter(LC %in% c(1,2,3)) %>%
  filter(lat >= -15, lat <= 10,
         lon >= -15, lon <= 60) %>%
  left_join(GridArea,
            by = c("lon","lat")) %>%
  na.omit()

threshold <- 0.7

df.seasonality <- All.Transitions.ref.wide %>%
  na.omit() %>%
  mutate(Delta_MCWD = value_MCWD - value.ref_MCWD) %>%
  mutate(seasonality = case_when(Delta_MCWD < 0 ~ "More",
                                 TRUE ~ "Less"))

df.agreement <- df.seasonality %>%
  group_by(scenario,model,period) %>%
  mutate(sign.Delta = case_when(Delta_MCWD == 0 ~ 1,
                                TRUE ~ sign(Delta_MCWD))) %>%
  group_by(scenario,period,lon,lat) %>%
  summarise(mode = modal(sign.Delta),
            N = length(sign.Delta),
            agreement = (sum(sign.Delta == mode) >=
                           threshold*N),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(seasonality = case_when(mode == -1 ~ "More",
                                 TRUE ~ "Less")) %>%
  left_join(df.seasonality %>%
              filter(model == "MEM") %>%
              mutate(change = (value_LC.pred != value.ref_LC.pred)) %>%
              dplyr::select(lon,lat,value.ref_LC.pred,period,scenario,change),
            by = c("lon","lat","period","scenario")) %>%
  mutate(value.ref_LC.pred = factor(value.ref_LC.pred,
                                    levels = c(2,3,1)))


segments <- expand.grid(
  scenario = unique(df.agreement$scenario),
  value.ref_LC.pred = factor(1:3,levels = c(2,3,1)),
  period = unique(df.agreement$period)) %>%
  mutate(x = 5, xend = 47,
         y = 0, yend = 0) %>%
  mutate(period = factor(period,
                         levels = c("Near_future",
                                    "Mid_future",
                                    "Long_future"))) %>%
  filter(period == "Long_future",
         scenario != "ssp534-over")

cAll.Transitions.agreement <- cAll.Transitions %>%
  left_join(df.agreement %>%
              dplyr::select("scenario","period",
                            "lon","lat","agreement"),
            by = c("scenario","period",
                   "lon","lat"))

df2plot <- cAll.Transitions.agreement %>%
  filter(model == "MEM",
         period == "Long_future",
         scenario != "ssp534-over") %>%
  mutate(change.agreement.seasonality = case_when(
    agreement & change ~ "Transition",
    !agreement & change ~ "Transition0",
    !agreement ~ "o",
    agreement & !change ~ seasonality,
    TRUE ~ "o")) %>%
  mutate(change.agreement.seasonality = factor(change.agreement.seasonality,
                                               levels = c("o","Less","More","Transition0","Transition")))

ggplot(data = df2plot) +
  geom_raster(aes(x = lon, y = lat,
                  fill = change.agreement.seasonality,
                  alpha = agreement)) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  geom_sf(data = rainforests,
          fill = NA, color = "black",
          linewidth = 0.5) +
  geom_segment(data = segments,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               inherit.aes = FALSE, linetype = 2,
               color = "grey12") +
  coord_sf(xlim = c(-15, 60),
           ylim = c(-15, 10), expand = FALSE) +
  theme_map() +
  scale_fill_manual(values = c(
    "#999999",
    "#56B4E9",
    "#E69F00",
    "#009E73",
    "#009E73")) +
  # facet_grid(scenario ~ value.ref_LC.pred) +
  facet_wrap( ~ scenario,
              ncol = 1) +
  scale_alpha_manual(values = c(0.5,1)) +
  guides(fill = "none", alpha = "none") +
  theme_map() +
  theme(strip.background = element_blank(),
        text = element_text(size = 20))

df.piechart <- df2plot %>%
  filter(period == "Long_future",
         model == "MEM",
         scenario != "ssp534-over") %>%
  group_by(scenario,agreement,period,value.ref_LC.pred,value_LC.pred,
           change.agreement.seasonality) %>%
  summarise(tot = sum(area*land.frac,
                      na.rm = TRUE)/1e12,
            .groups = "keep") %>%
  mutate(change.agreement.seasonality = factor(change.agreement.seasonality,
                                               levels = c("o","Less","More","Transition0","Transition")),
         value.ref_LC.pred = factor(value.ref_LC.pred,
                                    levels = c(2,3,1)))

df.piechart %>%
  filter(scenario == "ssp245") %>%
  group_by(value.ref_LC.pred) %>%
  summarise(sum(tot))

df.piechart %>%
  filter(scenario == "ssp245") %>%
  filter(change.agreement.seasonality %in% c("Transition","Transition0")) %>%
  pull(tot) %>%
  sum()

df.piechart %>%
  filter(scenario == "ssp245",
         agreement) %>%
  filter(change.agreement.seasonality %in% c("Transition","Transition0")) %>%
  pull(tot) %>%
  sum()

df.piechart %>%
  filter(scenario == "ssp245") %>%
  filter(change.agreement.seasonality %in% c("Less","More","o")) %>%
  pull(tot) %>%
  sum()

df.piechart %>%
  filter(scenario == "ssp245",
         agreement) %>%
  filter(change.agreement.seasonality %in% c("Less","More","o")) %>%
  pull(tot) %>%
  sum()


ggplot(data = df.piechart) +
  geom_bar(aes(x = value.ref_LC.pred, y = tot,
               fill = change.agreement.seasonality),
           position = position_stack(),
           stat = "identity") +
  facet_wrap(~ scenario, ncol = 1) +
  theme_bw() +
  scale_fill_manual(values = c(
    "#999999",
    "#56B4E9",
    "#E69F00",
    alpha("#009E73", 0.5) ,
    "#009E73"
  )) +
  guides(fill = "none") +
  labs(x = "", y = "") +
  scale_y_continuous(breaks = c(0,2.5,5)) +
  scale_alpha_manual(values = c(0.5,1)) +
  theme(text = element_text(size = 20),
        strip.background = element_blank())


df.agreement %>%
  left_join(LC,
            by = c("lon","lat")) %>%
  filter(LC %in% c(1,2,3)) %>%
  filter(scenario == "ssp245") %>%
  group_by(LC,period) %>%
  summarise(frac = sum(agreement)/length(agreement)*100)

saveRDS(cAll.Transitions %>%
          left_join(df.agreement %>%
                      dplyr::select(-c(value.ref_LC.pred,seasonality,change)),
                    by = c("scenario","period","lon","lat")),
        "./outputs/All.transitions.future.RDS")

#################################################################################


df2plot2 <- cAll.Transitions.agreement %>%
  filter(scenario != "ssp534-over") %>%
  mutate(change.agreement.seasonality = case_when(
    agreement & change ~ "Transition",
    !agreement & change ~ "Transition0",
    !agreement ~ "o",
    agreement & !change ~ seasonality,
    TRUE ~ "o")) %>%
  mutate(change.agreement.seasonality = factor(change.agreement.seasonality,
                                               levels = c("o","Less","More","Transition0","Transition")))


A <-  df2plot2 %>%
  group_by(change.agreement.seasonality,
           change, agreement,seasonality,
           LC,scenario,period,model) %>%
  summarise(N = n(),
            area = sum(area*land.frac)/1e12,
            .groups = "keep") %>%
  left_join(df.GW,
            by = c("scenario","model","period")) %>%
  mutate(period = factor(period,
                         levels = c("historical","Near_future","Mid_future","Long_future")))

A.mod <- A %>%
  filter(LC %in% c(1,2,3),
         period != "historical") %>%
  mutate(LC = factor(LC,
                     levels = c(2,3,1)))


df2plot.shitter <- A.mod %>%
  rowwise()  %>%
  mutate(gw = gw*(runif(1,0.95,1.05)))

df2plot.CI <- df2plot.shitter %>%
  filter(model == "MEM",
         !change) %>%
  left_join(df2plot.shitter %>%
              filter(model != "MEM") %>%
              group_by(LC,change.agreement.seasonality,scenario,period) %>%
              summarise(gw.low = quantile(gw,0.15,na.rm = TRUE),
                        gw.high = quantile(gw,0.85,na.rm = TRUE),
                        area.high = quantile(area,0.15,na.rm = TRUE),
                        area.low = quantile(area,0.85,na.rm = TRUE),
                        .groups = "keep"),
            by = c("LC","change.agreement.seasonality","scenario","period"))

ggplot(data = df2plot.CI,
       aes(x = gw, y = area,
           color = change.agreement.seasonality,
           fill = change.agreement.seasonality)) +
  geom_point(aes(shape = scenario)) +
  geom_errorbar(aes(ymin = area.low, ymax = area.high)) +
  geom_errorbar(aes(xmin = gw.low, xmax = gw.high)) +
  stat_smooth(method = "lm",
              fullrange = TRUE) +
  facet_grid( ~ LC) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "", y = "") +
  guides(shape = "none", color = "none", fill = "none") +
  scale_x_continuous(limits = c(0,6.2)) +
  scale_fill_manual(values = c(
    "#999999","#56B4E9","#E69F00")) +
  scale_color_manual(values = c(
    "#999999","#56B4E9","#E69F00")) +
  theme(text = element_text(size = 20),
        strip.text = element_blank(),
        strip.background = element_blank())

A.mod %>%
  filter(model == "MEM") %>%
  filter(!change) %>%
  group_by(change.agreement.seasonality,LC) %>%
  summarise(slope = summary(lm(area ~ gw))[["coefficients"]][2,1],
            p.val = summary(lm(area ~ gw))[["coefficients"]][2,4],
            r2 = summary(lm(area ~ gw))[["r.squared"]],
            .groups = "keep") %>%
  arrange(LC)

#################################################################################

df.transitions.sum <-
  df.MCWD.ts <- data.frame()

for (cLC in c(1,2,3)){

  df.gain <- cAll.Transitions.agreement %>%
    filter(value.ref_LC.pred != cLC,
           value_LC.pred == cLC,
           agreement) %>%
    group_by(scenario,period,model) %>%
    summarise(N = n(),
              area = sum(area*land.frac)/1e12,
              change = "gain",
              LC = cLC,
              .groups = "keep")

  df.loss <- cAll.Transitions.agreement %>%
    filter(value_LC.pred != cLC,
           value.ref_LC.pred == cLC,
           agreement) %>%
    group_by(scenario,period,model) %>%
    summarise(N = n(),
              area = sum(area*land.frac)/1e12,
              change = "loss",
              LC = cLC,
              .groups = "keep")

  df.net <- full_join(df.gain,
                      df.loss,
                      by = c("scenario","model","LC","period")) %>%
    group_by(scenario,period,model,LC) %>%
    summarise(N = N.x - N.y,
              area = area.x - area.y,
              change = "net",
              .groups = "keep")

  df.MCWD.ts <- bind_rows(df.MCWD.ts,
                          cAll.Transitions.agreement %>%
    filter(value.ref_LC.pred == value_LC.pred,
           value.ref_LC.pred == cLC,
           agreement) %>%
    mutate(Delta_MCWD = value_MCWD - value.ref_MCWD) %>%
    mutate(seasonality = case_when(Delta_MCWD < 0 ~ "More",
                                   TRUE ~ "Less")) %>%
    group_by(scenario,period,model,seasonality) %>%
    summarise(Delta_MCWD.m = mean(Delta_MCWD),
              Delta_MCWD.med = median(Delta_MCWD),
              MCWD_scenario.m = mean(value_MCWD),
              MCWD_scenario.median = median(value_MCWD),
              .groups = "keep") %>%
    mutate(LC = cLC))


  df.transitions.sum <- bind_rows(df.transitions.sum,
                                  df.net,
                                  df.loss,
                                  df.gain)


}

transitions.vs.gw <- df.transitions.sum %>%
  left_join(df.GW,
            by = c("scenario","model","period"))

# Area tot initial
cAll.Transitions.agreement %>%
  filter(model == "MEM",
         scenario == "ssp245",
         period == "Long_future") %>%
  filter(LC %in% c(1,2,3)) %>%
  group_by(value.ref_LC.pred) %>%
  summarise(area = sum(area*land.frac)/1e12)

# Area tot final
cAll.Transitions.agreement %>%
  filter(model == "MEM",
         scenario == "ssp245",
         period == "Long_future") %>%
  filter(LC %in% c(1,2,3)) %>%
  group_by(value_LC.pred) %>%
  summarise(area = sum(area*land.frac)/1e12)

# Transitions agree
cAll.Transitions.agreement %>%
  filter(model == "MEM",
         scenario == "ssp585",
         period == "Long_future",
         agreement) %>%
  filter(LC %in% c(1,2,3),
         value.ref_LC.pred != value_LC.pred) %>%
  group_by(value.ref_LC.pred,value_LC.pred) %>%
  summarise(area = sum(area*land.frac)/1e12)

# Transitions total (agree + disagree)
cAll.Transitions.agreement %>%
  filter(model == "MEM",
         scenario == "ssp585",
         period == "Long_future") %>%
  filter(LC %in% c(1,2,3),
         value.ref_LC.pred != value_LC.pred) %>%
  group_by(value.ref_LC.pred,value_LC.pred) %>%
  summarise(area = sum(area*land.frac)/1e12)


# Internal total
cAll.Transitions.agreement %>%
  filter(model == "MEM",
         scenario == "ssp585",
         period == "Long_future") %>%
  filter(value_LC.pred == value.ref_LC.pred) %>%
  filter(LC %in% c(1,2,3)) %>%
  mutate(seasonality.agreement = case_when(agreement & seasonality == "Less" ~ "Less",
                                           agreement & seasonality == "More" ~ "More",
                                           !agreement ~ "o")) %>%

  group_by(value_LC.pred) %>%
  summarise(area = sum(area*land.frac)/1e12)

# Internal per category
cAll.Transitions.agreement %>%
  filter(model == "MEM",
         scenario == "ssp585",
         period == "Long_future") %>%
  filter(value_LC.pred == value.ref_LC.pred) %>%
  filter(LC %in% c(1,2,3)) %>%
  mutate(seasonality.agreement = case_when(agreement & seasonality == "Less" ~ "Less",
                                           agreement & seasonality == "More" ~ "More",
                                           !agreement ~ "o")) %>%

  group_by(value_LC.pred,seasonality.agreement) %>%
  summarise(area = sum(area*land.frac)/1e12)


ggplot(data = cAll.Transitions %>%
         filter(model == "MEM",
                scenario != "ssp534-over",
                period == "Long_future") %>%
         filter(LC %in% c(1,2,3))) +
  geom_raster(aes(x = lon, y = lat,
                  fill = as.factor(value.ref_LC.pred))) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  facet_wrap(~ scenario) +
  scale_x_continuous(limits = c(-15,60)) +
  scale_y_continuous(limits = c(-15,10)) +
  theme_bw()


cAll.Transitions %>%
  filter(model == "MEM",
         scenario == "ssp245",
         period == "Long_future") %>%
  group_by(scenario,value.ref_LC.pred) %>%
  summarise(area = sum(area*land.frac)/1e12)


df2plotAplot <- transitions.vs.gw %>%
  filter(change %in% c("loss","gain"),
         scenario != c("ssp534-over")) %>%
  ungroup()
df2plotAplot <- df2plotAplot %>%
  complete(scenario = unique(df2plotAplot$scenario),
           period = c("historical",unique(df2plotAplot$period)),
           model = unique(df2plotAplot$model),
           LC = unique(df2plotAplot$LC),
           change = unique(df2plotAplot$change),
           fill = list(area = 0,
                       N = 0)) %>%
  left_join(df.GW.hist %>%
              dplyr::select(-scenario),
            by = c("period","model")) %>%
  mutate(gw = case_when(is.na(gw.x) ~ gw.y,
                        TRUE ~ gw.x))


df2plotAplot.shitter <- df2plotAplot %>%
  rowwise()  %>%
  mutate(gw = gw*(runif(1,0.95,1.05)))

df2plotAplot.CI <- df2plotAplot.shitter %>%
  filter(model == "MEM") %>%
  left_join(df2plotAplot.shitter %>%
              filter(model != "MEM") %>%
              group_by(LC,scenario,period,change) %>%
              summarise(gw.low = quantile(gw,0.15,na.rm = TRUE),
                        gw.high = quantile(gw,0.85,na.rm = TRUE),
                        area.high = quantile(area,0.15,na.rm = TRUE),
                        area.low = quantile(area,0.85,na.rm = TRUE),
                        .groups = "keep"),
            by = c("LC","scenario","period","change"))

Aplot <- ggplot(data =  df2plotAplot.CI %>%
                  filter(!(change == "gain" & LC == 1)),
                aes(x = gw, y = area,
                    group = interaction(LC,change),
                    color = as.factor(LC), fill = as.factor(LC))) +
  geom_hline(yintercept = 0,
             linetype = 2) +
  geom_point(aes(shape = scenario),
             size = 2) +
  geom_errorbar(aes(ymin = area.low, ymax = area.high)) +
  geom_errorbar(aes(xmin = gw.low, xmax = gw.high)) +
  stat_smooth(method = "lm",
              fullrange = TRUE) +
  facet_wrap(~ change) +
  scale_color_manual(values = c("#c49402","#005401","#448704","grey")) +
  scale_fill_manual(values = c("#c49402","#005401","#448704","grey")) +
  theme_bw() +
  guides(color = "none",fill = "none") +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        text = element_text(size = 20)) +
  # scale_y_continuous(limits = c(0,1.5)) +
  labs(x = '',y = "")

Aplot

transitions.vs.gw %>%
  na.omit() %>%
  filter(change %in% c("loss","gain"),
         scenario != c("ssp534-over"),
         model == "MEM") %>%
  group_by(change,LC) %>%
  mutate(N = n()) %>%
  filter(N > 2) %>%
  summarise(slope = summary(lm(area ~ gw))[["coefficients"]][2,1],
            p.val = summary(lm(area ~ gw))[["coefficients"]][2,4],
            r2 = summary(lm(area ~ gw))[["r.squared"]],
            .groups = "keep")

df.MCWD.ts <- df.MCWD.ts %>%
  complete(scenario = unique(df.MCWD.ts$scenario),
           period = c("historical",unique(df.MCWD.ts$period)),
           seasonality = c(unique(df.MCWD.ts$seasonality)),
           model = unique(df.MCWD.ts$model),
           LC = unique(df.MCWD.ts$LC),
           fill = list(Delta_MCWD.m = 0))


df.MCWD.ts.vs.gw <- df.MCWD.ts %>%
  left_join(df.GW,
            by = c("scenario","model","period")) %>%
  left_join(df.GW.hist %>%
              dplyr::select(-scenario),
            by = c("period","model")) %>%
  mutate(gw = case_when(is.na(gw.x) ~ gw.y,
                        TRUE ~ gw.x))


extr.temp <- df.GW %>%
  filter(scenario != "ssp534-over") %>%
  summarise(Min = min(gw,na.rm = TRUE),
            Max = max(gw,na.rm = TRUE),
            .groups = "keep")

df.MCWD.ts.vs.gw.shitter <- df.MCWD.ts.vs.gw %>%
  rowwise()  %>%
  mutate(gw = gw*(runif(1,0.95,1.05)))

df2plotBplot.CI <- df.MCWD.ts.vs.gw.shitter %>%
  filter(model == "MEM",
         scenario != "ssp534-over") %>%
  left_join(df.MCWD.ts.vs.gw.shitter %>%
              filter(model != "MEM") %>%
              group_by(LC,scenario,seasonality,period) %>%
              summarise(gw.low = quantile(gw,0.15,na.rm = TRUE),
                        gw.high = quantile(gw,0.85,na.rm = TRUE),
                        Delta_MCWD.m.high = quantile(Delta_MCWD.m,0.15,na.rm = TRUE),
                        Delta_MCWD.m.low = quantile(Delta_MCWD.m,0.85,na.rm = TRUE),
                        .groups = "keep"),
            by = c("LC","scenario","period","seasonality"))

Bplot <- ggplot(data = df2plotBplot.CI,
       aes(x = gw, y = Delta_MCWD.m,
           group = interaction(seasonality,LC),
           color = as.factor(LC),
           fill = as.factor(LC))) +
  geom_hline(yintercept = 0,
             linetype = 2) +
  geom_point(aes(shape = scenario)) +
  geom_errorbar(aes(ymin = Delta_MCWD.m.low, ymax = Delta_MCWD.m.high),
                alpha = 0.5) +
  geom_errorbar(aes(xmin = gw.low, xmax = gw.high),
                alpha = 0.5) +
  stat_smooth(method = "lm", formula = y ~ poly(x,1),
              fullrange = T) +
  scale_color_manual(values = c("#c49402","#005401","#448704","grey")) +
  scale_fill_manual(values = c("#c49402","#005401","#448704","grey")) +
  theme_bw() +
  guides(color = "none", fill = "none",shape = "none") +
  theme(text = element_text(size = 20)) +
  labs(x = "", y = "")

Bplot

df.MCWD.ts.vs.gw %>%
  filter(period == "Long_future",
         scenario == "ssp585",
         model == "MEM")

df.MCWD.ts.vs.gw %>%
  filter(model == "MEM") %>%
  group_by(seasonality,LC) %>%
  summarise(slope = summary(lm(Delta_MCWD.m ~ gw))[["coefficients"]][2,1],
            p.val = summary(lm(Delta_MCWD.m ~ gw))[["coefficients"]][2,4],
            r2 = summary(lm(Delta_MCWD.m ~ gw))[["r.squared"]],
            .groups = "keep")

df.MCWD.ts.vs.gw %>%
  filter(model == "MEM") %>%
  filter(scenario %in% c("ssp245","ssp585"),
         period == "Long_future") %>%
  dplyr::select(scenario,model,period,gw) %>%
  distinct()

