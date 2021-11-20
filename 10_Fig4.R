




# Created 19.11.2021


# Fig4


####################################################################



library(dplyr)
library(ggplot2)
library(sf)
library(viridis)
library(patchwork)


setwd("E:/Postdoc Imperial/Projects/COVID19 Greece/TutorialExcessOutput/")
load("Italy.RData")




# First pannel
ggplot() + geom_sf(data = d$province$age$`40<`, aes(fill = exceedance.REM), size = 0.2) + 
  scale_fill_viridis_c(name = "", limits = c(0, 1)) + theme_light() +
  ggtitle("A. 40<") + 
  theme(legend.key.width=unit(0.3,"cm"), 
        text = element_text(size=10)) -> L1


ggplot() + geom_sf(data = d$province$age$`40-59`, aes(fill = exceedance.REM), size = 0.2) + 
  scale_fill_viridis_c(name = "", limits = c(0, 1)) + theme_light() +
  ggtitle("B. 40-59") + 
  theme(legend.key.width=unit(0.3,"cm"),
        text = element_text(size=10)) -> L2


ggplot() + geom_sf(data = d$province$age$`60-69`, aes(fill = exceedance.REM), size = 0.2) + 
  scale_fill_viridis_c(name = "", limits = c(0, 1)) + theme_light() +
  ggtitle("C. 60-69") + 
  theme(legend.key.width=unit(0.3,"cm"),
        text = element_text(size=10)) -> L3

ggplot() + geom_sf(data = d$province$age$`70-79`, aes(fill = exceedance.REM), size = 0.2) + 
  scale_fill_viridis_c(name = "", limits = c(0, 1)) + theme_light() +
  ggtitle("D. 70-79") + 
  theme(legend.key.width=unit(0.3,"cm"),
        text = element_text(size=10)) -> L4

ggplot() + geom_sf(data = d$province$age$`80+`, aes(fill = exceedance.REM), size = 0.2) + 
  scale_fill_viridis_c(name = "", limits = c(0, 1)) + theme_light() +
  ggtitle("E. 80>") + 
  theme(legend.key.width=unit(0.3,"cm"),
        text = element_text(size=10)) -> L5


png("PosteriorProb.png", width = 22, height = 15, res = 300, units = "cm")
print(
(L1|L2|L3)/(L4|L5) + plot_annotation(title = "Posterior probability that REM>0 by age group") + 
  theme(text = element_text(size=8)) 
)
dev.off()



####################################################################
####################################################################
####################################################################
####################################################################






