

# Created 9.10.2021

# Clean the mortality file in Italy. 

#-----------------------------------------------------------------

# One can download the mortality data for 2015-2020 at 
# https://www.istat.it/it/archivio/240401



setwd("E:/Postdoc Imperial/Projects/COVID19 Greece/TutorialExcess/")


installpack <- FALSE


if(installpack){
  install.packages(c("readr", "dplyr", "tidyr"))
}



library(readr)
library(dplyr)
library(tidyr)



deaths <- read_csv("Desktop/Dataset-decessi-comunali-giornalieri-e-tracciato-record_5marzo/comuni_giornaliero_31dicembre.csv")

# subset the dataset
deaths %>% select_at(
  vars("PROV", "NOME_PROVINCIA", "CL_ETA", "GE",
                  paste0("M_", 15:20), 
                  paste0("F_", 15:20))
  )-> deaths


# Change to long format
deaths <- gather(deaths, agesex, deaths, M_15:F_20, factor_key=TRUE)
deaths %>% mutate(
  sex = substr(agesex, start = 1, stop = 1),
  year = as.numeric(paste0("20",substr(agesex, start = 3, stop = 4)))
) -> deaths

deaths$agesex <- NULL

# Fix the age. The CL_ETA is the age variable denoting the following age groups:

# 0=0
# 1=1-4
# 2=5-9
# 3=10-14
# 4=15-19
# 5=20-24
# 6=25-29
# 7=30-34
# 8=35-39
# 9=40-44
# 10=45-49
# 11=50-54
# 12=55-59
# 13=60-64
# 14=65-69
# 15=70-74
# 16=75-79
# 17=80-84
# 18=85-89
# 19=90-94
# 20=95-99
# 21=100+
# see also https://www.istat.it/it/archivio/240401



deaths$ageg <- NA
deaths$ageg[deaths$CL_ETA %in% 0:8] <- "less40"
deaths$ageg[deaths$CL_ETA %in% 9:12] <- "40-59"
deaths$ageg[deaths$CL_ETA %in% 13:14] <- "60-69"
deaths$ageg[deaths$CL_ETA %in% 15:16] <- "70-79"
deaths$ageg[deaths$CL_ETA %in% 17:21] <- "80plus"
deaths$CL_ETA <- NULL


# Fix the date

deaths %>% mutate(
  date = paste0(year, "-", 
                substr(GE, start = 1, stop = 2), "-", 
                substr(GE, start = 3, stop = 4))
) %>% mutate(date = as.Date(date)) -> deaths

deaths$date <- paste0(
  
)

# Aggregate by ISO week and age group
deaths %>% group_by(PROV, NOME_PROVINCIA, sex, year, ageg) %>% 
  summarise(deaths = sum(deaths)) -> deaths
