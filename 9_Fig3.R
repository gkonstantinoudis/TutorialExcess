




# Created 19.11.2021


# Fig3


####################################################################

library(dplyr)
library(ggplot2)
library(sf)
library(viridis)
library(lubridate)
library(ISOweek)
library(patchwork)


setwd("E:/Postdoc Imperial/Projects/COVID19 Greece/TutorialExcessOutput/")
load("Italy.RData")

# get the shp
shp <- read_sf("ProvCM01012020_g_WGS84.shp")
link_table <- readRDS("link_table")

# and for NUTS2
shp %>% left_join(., link_table, by = c("SIGLA" = "SIGLA")) %>% 
  group_by(NAMNUTS2) %>% 
  summarise() -> shp_NUTS2
  

# get the iso correspondance between months for the xaxis

date.iso <- data.frame(
  iso = rep(d_week$country$sex$F$EURO_LABEL, each = 7),
  date = ISOweek2date(unlist(lapply(d_week$country$sex$F$EURO_LABEL, function(X) paste(X, 1:7, sep = "-"))))
)

date.iso$month <- month(date.iso$date)
date.iso$month_nam <- month.abb[date.iso$month]

d_week$country$sex$M %>% 
  left_join(., date.iso, by = c("EURO_LABEL" = "iso")) %>% 
  mutate(x = as.numeric(as.factor(EURO_LABEL))) -> b

b <-  b[, c("month_nam","x")]
b <- b[-c(1:2),]

b <- b[!duplicated(b$month_nam),]







# First pannel
ggplot() + geom_sf(data = d$country$none, aes(fill = round(median.REM, digits = 2)), col = "red") + 
  scale_fill_viridis_c(name = "", limits = c(-5, 47)) + theme_light() +
  ggtitle("1A. National")-> L1



d_week$country$sex$M %>% 
  mutate(x = as.numeric(as.factor(EURO_LABEL))) %>% 
  
  ggplot() + geom_line(aes(x = x, y = median.REM)) + 
  geom_point(aes(x = x, y = median.REM), size = 1) + 
  geom_ribbon(aes(x = x, ymin = low.REM, ymax = upp.REM), alpha = 0.4, fill = viridis(15)[5]) +
  geom_hline(yintercept = 0, col = "red", linetype = 2) + 
  theme_light() + ylim(c(-50, 130)) + ggtitle("1B. Males") + ylab("") + 
  scale_x_continuous(minor_breaks = NULL, breaks = b$x, labels = b$month_nam) -> P11



d_week$country$sex$F %>% mutate(x = as.numeric(as.factor(EURO_LABEL))) %>% 
  
  ggplot() + geom_line(aes(x = x, y = median.REM)) + 
  geom_point(aes(x = x, y = median.REM), size = 1) + 
  geom_ribbon(aes(x = x, ymin = low.REM, ymax = upp.REM), alpha = 0.4, fill = viridis(15)[5]) +
  geom_hline(yintercept = 0, col = "red", linetype = 2) + 
  theme_light() + ylim(c(-50, 130)) + ggtitle("1C. Females") + ylab("") + 
  scale_x_continuous(minor_breaks = NULL, breaks = b$x, labels = b$month_nam) -> P12

# P1/P2

L1|(P11/P12) -> M1


# Second pannel

region <- d$region$none %>% filter(median.REM == max(median.REM)) %>% select(NAMNUTS2)
region$geometry <- NULL
region <- "Veneto"




d$region$none %>% filter(NAMNUTS2 %in% "Veneto") %>% 
  select(geometry) %>% 
ggplot() +  
  geom_sf(data = d$region$none, aes(fill = round(median.REM, digits = 2))) + 
  geom_sf(fill = NA, col = "red", size = 1) + 
  scale_fill_viridis_c(name = "", limits = c(-5, 47)) + theme_light() + 
  ggtitle(paste0("2A. NUTS2 regions: ", region)) -> L2


d_week$region$sex$M %>% filter(NAMNUTS2 %in% region) %>% 
  mutate(x = as.numeric(as.factor(EURO_LABEL))) %>% 
  
  ggplot() + geom_line(aes(x = x, y = median.REM)) + 
  geom_point(aes(x = x, y = median.REM), size = 1) + 
  geom_ribbon(aes(x = x, ymin = low.REM, ymax = upp.REM), alpha = 0.4, fill = viridis(15)[5]) +
  geom_hline(yintercept = 0, col = "red", linetype = 2) + 
  theme_light() + ylim(c(-50, 130)) + ggtitle("2B. Males") + ylab("") + 
  scale_x_continuous(minor_breaks = NULL, breaks = b$x, labels = b$month_nam) -> P21



d_week$region$sex$F %>% filter(NAMNUTS2 %in% region) %>% 
  mutate(x = as.numeric(as.factor(EURO_LABEL))) %>% 
  
  ggplot() + geom_line(aes(x = x, y = median.REM)) + 
  geom_point(aes(x = x, y = median.REM), size = 1) + 
  geom_ribbon(aes(x = x, ymin = low.REM, ymax = upp.REM), alpha = 0.4, fill = viridis(15)[5]) +
  geom_hline(yintercept = 0, col = "red", linetype = 2) + 
  theme_light() + ylim(c(-50, 130)) + ggtitle("2C. Females") + ylab("") + 
  scale_x_continuous(minor_breaks = NULL, breaks = b$x, labels = b$month_nam) -> P22


L2|(P21/P22) -> M2








# Third pannel

prov <- d$province$none %>% filter(median.REM == max(median.REM)) %>% select(ID_space)
prov$geometry <- NULL
prov <- "VE"
prov.name <- "Venice"

d$province$none %>% filter(SIGLA == prov) %>% 
  select(geometry) %>% 
  ggplot() +  
  geom_sf(data = d$province$none, aes(fill = round(median.REM, digits = 2))) + 
  geom_sf(fill = NA, col = "red", size = .8) + 
  scale_fill_viridis_c(name = "", limits = c(-5, 47)) + theme_light() +
  ggtitle(paste0("3A. NUTS3 regions: ", prov.name)) -> L3



prov <- d$province$none %>% filter(SIGLA == prov) %>% select(ID_space)
prov$geometry <- NULL

d_week$province$sex$M %>% filter(ID_space %in% prov) %>% 
  mutate(x = as.numeric(as.factor(EURO_LABEL))) %>% 
  
  ggplot() + geom_line(aes(x = x, y = median.REM)) + 
  geom_point(aes(x = x, y = median.REM), size = 1) + 
  geom_ribbon(aes(x = x, ymin = low.REM, ymax = upp.REM), alpha = 0.4, fill = viridis(15)[5]) +
  geom_hline(yintercept = 0, col = "red", linetype = 2) + 
  theme_light() + ylim(c(-50, 130)) + ggtitle("3B. Males") + ylab("") + 
  scale_x_continuous(minor_breaks = NULL, breaks = b$x, labels = b$month_nam) -> P31



d_week$province$sex$F %>% filter(ID_space %in% prov) %>% 
  mutate(x = as.numeric(as.factor(EURO_LABEL))) %>% 
  
  ggplot() + geom_line(aes(x = x, y = median.REM)) + 
  geom_point(aes(x = x, y = median.REM), size = 1) + 
  geom_ribbon(aes(x = x, ymin = low.REM, ymax = upp.REM), alpha = 0.4, fill = viridis(15)[5]) +
  geom_hline(yintercept = 0, col = "red", linetype = 2) + 
  theme_light() + ylim(c(-50, 130)) + ggtitle("3C. Females") + ylab("") + 
  scale_x_continuous(minor_breaks = NULL, breaks = b$x, labels = b$month_nam) -> P32


L3|(P31/P32) -> M3

png("SpatiotemporalRegions.png", width = 25, height = 30, res = 300, units = "cm")
print(
M1/M2/M3 + plot_annotation(title = "Percentage relative excess mortality")
)
dev.off()




####################################################################
####################################################################
####################################################################
####################################################################
####################################################################


