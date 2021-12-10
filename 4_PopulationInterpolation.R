

# Created 08.10.2021


# Calculate weekly population


#---------------------------------------------------------------------------------


setwd("E:/Postdoc Imperial/Projects/COVID19 Greece/TutorialExcessOutput/")


installpack <- FALSE


if(installpack){
  install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "patchwork"))
}




library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)



load("Output/pop15_20_final.RData")
pop <- pop15_20; rm(pop15_20)



# the ISO weeks file
EUROSTAT <- read_excel("EUROSTAT_ISO.xls")
EUROSTAT$EURO_TIME <- as.Date(format(as.POSIXct(EUROSTAT$EURO_TIME, format='%Y-%m-%d %H:%M:%S'), format='%Y-%m-%d'))
EUROSTAT$year <- as.numeric(format(EUROSTAT$EURO_TIME, "%Y"))
EUROSTAT %>% filter(year <= 2020 & year>= 2015) -> EUROSTAT




# the population file should have the following format:
# NUTS318CD   ageg  sex year population
# 1         1 less40 male 2015      64769
# 2         1 less40 male 2016      62578
# 3         1 less40 male 2017      68788
# 4         1 less40 male 2018      62038
# 5         1 less40 male 2019      67761
# 6         1 less40 male 2020      60105
head(pop)



expand.grid(age = c("less40", "40-59", "60-69", "70-79", "80plus"), 
            sex = c("male", "female"), 
            region = unique(pop$NUTS318CD), 
            EURO_LABEL = unique(EUROSTAT$EURO_LABEL)) -> pop_weekly



pop_weekly$EURO_LABEL <- as.character(pop_weekly$EURO_LABEL)


EUROSTAT$EURO_TIME <- as.Date(format(as.POSIXct(EUROSTAT$EURO_TIME, format='%Y-%m-%d %H:%M:%S'), format='%Y-%m-%d'))
EUROSTAT$year <- as.numeric(format(EUROSTAT$EURO_TIME, "%Y"))

EUROSTAT %>% filter(year <= 2020) %>% group_by(EURO_LABEL) %>% filter(row_number()==4) -> EUROSTAT
EUROSTAT %>% group_by(year) %>% mutate(refdate = as.Date(paste0(year, "-01-01"))) -> EUROSTAT
EUROSTAT$day2pred <- as.numeric(EUROSTAT$EURO_TIME - as.Date("2015-01-01") + 1)



pop_weekly <- left_join(pop_weekly, EUROSTAT[,c("EURO_LABEL", "year", "day2pred", "refdate")], 
                        by = c("EURO_LABEL" = "EURO_LABEL"))



# and now merge with the population

pop_weekly <- left_join(pop_weekly, pop, by = c("year" = "year", "age" = "ageg", "sex" = "sex", "region" = "NUTS318CD"))



## First two panels of the plot
pop %>% group_by(NUTS318CD, sex, ageg) %>% 
  summarise(coef = as.vector(coef(lm(population ~ year)))) %>% 
  filter(NUTS318CD %in% "VE", sex %in% "female", ageg %in% "40-59") -> coef.VE

pop %>% filter(NUTS318CD %in% "VE", sex %in% "female", ageg %in% "40-59") -> pop.VE

ggplot() + geom_point(data = pop.VE, aes(x=year, y=population)) + 
  geom_point(aes(x=2021, y = coef.VE$coef %*% c(1, 2021)), col = "red") + 
  geom_line(aes(x=2015:2021, y = c(coef.VE$coef[1] + coef.VE$coef[2]*2015:2021)), linetype = 2, col = "red") + 
  scale_x_continuous(breaks = 2015:2021) + ylim(c(136400, 139200)) +
  theme_light() +
  ggtitle("A.") + 
  theme(text = element_text(size = 8), 
        plot.margin=unit(c(0,0,0,0), "cm")) -> p1


ggplot() + geom_point(data = pop.VE, aes(x=year, y=population)) + 
  geom_point(aes(x=2021, y = coef.VE$coef %*% c(1, 2021)), col = "red") + 
  geom_line(aes(x=2015:2021, y = c(pop.VE$population, coef.VE$coef %*% c(1, 2021))), linetype = 1, col = "black") + 
  scale_x_continuous(breaks = 2015:2021) + ylim(c(136400, 139200)) + 
  theme_light() + geom_rect(aes(xmin = 2014.9, xmax = 2016.1, ymin = 138300, ymax = 139000),
                            fill = "transparent", color = "blue", size = 0.5) + 
  ggtitle("B.") + 
  theme(text = element_text(size = 8), 
        plot.margin=unit(c(0,0,0,0), "cm"))-> p2
##--##



# we do not have the population for 2021, we can calculate with linear interpolation
pop %>% group_by(NUTS318CD, sex, ageg) %>% 
  summarise(population = as.vector(coef(lm(population ~ year)) %*% c(1, 2021))) %>% 
  mutate(year = 2021) -> pop2021

pop$X <- NULL
pop2021 <- pop2021[,colnames(pop)]
pop <- rbind(pop, pop2021)
length(unique(pop$ageg))*length(unique(pop$year))*length(unique(pop$sex))*length(unique(pop$NUTS318CD))
# looks correct

# need to add the population of next year
pop$year <- pop$year - 1
colnames(pop)[5] <- "pop.next.year"

pop_weekly <- left_join(pop_weekly, pop, by = c("year" = "year", "age" = "ageg", "sex" = "sex", "region" = "NUTS318CD"))

# get the next years ref date
pop_weekly$refdate2 <- as.Date(paste0(pop_weekly$year + 1, "-01-01"))
pop_weekly$X <- NULL

pop_weekly %>% mutate(lambda = (pop.next.year - population)/as.numeric((refdate2 - refdate))) %>% 
  mutate(beta0 = population - lambda*as.numeric(refdate - as.Date("2015-01-01") + 1)) %>% 
  mutate(popfin = beta0 + lambda*day2pred) -> pop_weekly



pop_weekly$day2pred <- pop_weekly$refdate <- pop_weekly$population <- pop_weekly$pop.next.year <- pop_weekly$refdate2 <-
  pop_weekly$lambda <- pop_weekly$beta0 <-  pop_weekly$days2plot <- NULL

colnames(pop_weekly)[3] <- "NUTS318CD"
colnames(pop_weekly)[6] <- "population"

saveRDS(pop_weekly, file = "Output/pop_weekly")


# For the plot we are focusing on Veneto and to female population less than 40

# third panel of the plot

dat_weekly_VE <- pop_weekly %>% filter(NUTS318CD  %in% "VE", sex %in% "female", age %in% "40-59", year == 2015) %>% 
  mutate(x = as.numeric(as.factor(EURO_LABEL)))

ggplot() + geom_point(data = dat_weekly_VE, aes(x=x, y=population)) + 
  scale_x_continuous(breaks = dat_weekly_VE$x, labels = dat_weekly_VE$EURO_LABEL, expand = c(0.02, 0.02)) + 
  theme_light() + ylim(c(138300, 139000)) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 1, vjust = 0.2)) + 
  geom_rect(aes(xmin = 0.6, xmax = 53.4, ymin = 138300, ymax = 139000), fill = "transparent", color = "blue", size = 0.5) + 
  ggtitle("C.") + xlab("Estimated weekly population during 2015-2016") + 
  theme(text = element_text(size = 8), 
        plot.margin=unit(c(0,0,0,0), "cm")) -> p3
   
  
# Bring plots together

png("PopulationPlot.png", width = 17, height = 14, units = "cm", res = 300)
(p1|p2)/p3
dev.off()



######################################################################################
######################################################################################
######################################################################################
######################################################################################
