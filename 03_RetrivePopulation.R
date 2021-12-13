


# Created 08.10.2021

# Clean and download population


#---------------------------------------------------------------------------------


setwd("E:/Postdoc Imperial/Projects/COVID19 Greece/TutorialExcessOutput/")


installpack <- FALSE


if(installpack){
  install.packages(c("tidyverse", "reshape2", "lubridate", "rgdal", "spdep", "readr", "tidyr"))
}



library(tidyverse)
library(reshape2)
library(lubridate)
library(rgdal)
library(spdep)
library(readr)
library(tidyr)



# Population for January 1st 2020 can be downloaded from: http://demo.istat.it/popres/download.php?anno=2020&lingua=ita
# selecting Province on the bottom right of the page. Save this object as POP2020.

pop20 <- read_csv("POP2020.csv")
colnames(pop20) <- pop20[1,]
pop20 <- pop20[-1,]


pop20 = pop20 %>% select(`Codice provincia`, `Provincia`, `Totale Maschi`, `Totale Femmine`, `Età`)


pop20 %>% select(`Codice provincia`, `Provincia`, `Totale Maschi`, `Età`) %>% 
  mutate(sex = "M") %>% 
  rename(Value := `Totale Maschi`) %>% 
  rbind(., 
        pop20 %>% select(`Codice provincia`, `Provincia`, `Totale Femmine`, `Età`) %>% 
          mutate(sex = "F") %>% 
          rename(Value := `Totale Femmine`)) %>% 
  rename(Code := `Codice provincia`, 
         Province = Provincia, 
         Age := `Età`) -> pop20


# and aggregate by the selected age groups
pop20 %>% 
  mutate(Age = as.numeric(Age)) %>% # remove NAs since they reflect the Totals
  filter(!is.na(Age)) %>% 
  mutate(
  Age = cut(Age, breaks = c(-1, 39, 59, 69, 79, 101), 
            labels = c("less40", "40-59", "60-69", "70-79", "80plus"))
) %>% 
  group_by(Code, Province, Age, sex) %>% 
  summarise(pop = sum(as.numeric(Value))) %>% 
  mutate(year = 2020) -> pop20




# Population for the years 2015-2019 is available here: http://demo.istat.it/ricostruzione/download.php?lingua=ita
# Select the second Province link as you read the page from the top and name it POP2002_2019

pop15_19 <- read_delim("POP2002_2019.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE, 
                              skip = 4)

# We are interested in all nationalities and the years 2015:2019
pop15_19 <- pop15_19[(which(pop15_19$`Territorio/Età` == "Tutte le cittadinanze - Anno: 2015")):
                       (which(pop15_19$`Territorio/Età` == "Cittadinanza italiana - Anno: 2002")), ]


n.dat <- length(unique(as.numeric(pop15_19$`Territorio/Età`)))



# Seperate by sex and bring together
lapply(c("Maschi", "Femmine"), function(Y){
  
  lapply(which(pop15_19$`0` %in% Y), function(X) seq(from = X+1, to  = X + n.dat-1, by = 1)) -> list.sex
  
  pop15_19_sex <- NULL
  
  for(i in 1:length(list.sex)){
    pop15_19_sex_loop <- pop15_19[list.sex[[i]],]
    pop15_19_sex_loop$year <- 2014+i
    pop15_19_sex <- rbind(pop15_19_sex, pop15_19_sex_loop)
  }
  
  return(pop15_19_sex)
}
) -> pop.sex
  
pop.sex[[1]]$sex <- "M"
pop.sex[[2]]$sex <- "F"

pop15_19 <- rbind(pop.sex[[1]], pop.sex[[2]])

# and make long format
pop15_19 <- gather(pop15_19, Age, pop, `0`:`100`)
colnames(pop15_19)[c(1:2)] <- c("Code", "Province")
pop15_19 <- pop15_19[,colnames(pop20)]


# aggregate by age group
pop15_19 %>% 
  mutate(Age = as.numeric(Age)) %>% 
  mutate(
    Age = cut(Age, breaks = c(-1, 39, 59, 69, 79, 101), 
              labels = c("less40", "40-59", "60-69", "70-79", "80plus"))
  ) %>% 
  group_by(Code, Province, Age, sex, year) %>% 
  summarise(pop = sum(as.numeric(pop))) -> pop15_19


pop15_20 <- rbind(pop15_19, pop20)








# Fix problems with province names (with respect to the shapefile names)
pop15_20$Province[pop15_20$Province=="Valle d'Aosta/Vallée d'Aoste"] = "Aosta"
pop15_20$Province[pop15_20$Province=="Bolzano/Bozen"] = "Bolzano"
pop15_20$Province[pop15_20$Province=="Forlì-Cesena"] = "Forli'-Cesena"
pop15_20$Province[pop15_20$Province=="Massa-Carrara"] = "Massa Carrara"





# GeograpProvincermation from the shapefile
prov.shp = readOGR("ProvCM01012020_g_WGS84.shp")
geodata = prov.shp@data %>% dplyr::select(COD_RIP,COD_REG,COD_PROV,DEN_UTS,SIGLA)


# Merge pop and geodata to have it in compatible format
pop15_20 = left_join(pop15_20, geodata, by=c("Province"="DEN_UTS"))



# the population file should have the following format, so the population interpolation file
# runs smoothly

# NUTS318CD   ageg  sex year population
# 1         1 less40 male 2015      64769
# 2         1 less40 male 2016      62578
# 3         1 less40 male 2017      68788
# 4         1 less40 male 2018      62038
# 5         1 less40 male 2019      67761
# 6         1 less40 male 2020      60105

colnames(pop15_20)
pop15_20 <- pop15_20[, c("SIGLA", "Age", "sex", "year", "pop")]
colnames(pop15_20) <- c("NUTS318CD", "ageg", "sex", "year", "population")
pop15_20$sex[pop15_20$sex %in% "M"] <- "male"
pop15_20$sex[pop15_20$sex %in% "F"] <- "female"

# Save data
save(pop15_20, file="pop15_20_final.RData")




##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################