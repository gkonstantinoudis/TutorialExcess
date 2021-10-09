

# Created 08.10.2021


# Calculate weekly population


#---------------------------------------------------------------------------------


setwd("E:/Postdoc Imperial/Projects/COVID19 Greece/TutorialExcess/")


installpack <- FALSE


if(installpack){
  install.packages(c("readxl", "dplyr", "tidyr"))
}




library(readxl)
library(dplyr)
library(tidyr)




load("E:/Postdoc Imperial/Projects/COVID19 Greece/TutorialExcess/pop15_20_final.RData")
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

saveRDS(pop_weekly, file = "pop_weekly")





######################################################################################
######################################################################################
######################################################################################
######################################################################################
