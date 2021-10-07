


# Created 07.10.2021


#---------------------------------------------------------------------------------

setwd("E:/Postdoc Imperial/Projects/COVID19 Greece/TutorialExcess/")






# Step 1. Download temperature data from ERA5

# install required packages
install.packages(c("ecmwfr"))

# load packages
library(ecmwfr)

# You need to create an account here https://cds.climate.copernicus.eu/cdsapp#!/home, 
# and once you are ok and logged in, click on your name on the top right next to logout
# and retrieve the information about the API key.

# cds.key <- "c0edac92-9dee-4695-8063-eebb3ace3b27"
# wf_set_key(user = "52967", key = cds.key, service = "cds")

cds.key <- "Insert_your_CDS_API_KEY_here"
wf_set_key(user = "Insert_your_CDS_UID_here", key = cds.key, service = "cds")

request <- list(
  dataset_short_name = "reanalysis-era5-single-levels",
  product_type   = "reanalysis",
  format = "netcdf",
  variable = "2m_temperature",
  year = c("2015", "2016", "2017", "2018", "2019", "2020"),
  month = c(paste0("0", 1:9), 10:12),
  day = "16",
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", 
           "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", 
           "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  # area is specified as N, W, S, E
  area = c(48, 6, 34, 20),
  target = "temperature2015_2020_Italy.nc"
)

file <- wf_request(user = "52967",
                   request = request,
                   transfer = TRUE,
                   path = "~",
                   time_out = 3600*12,
                   verbose = TRUE)


# and you will get a temperature_Italy.nc file on your working directory. The temperature_Italy.nc file 
# is also provided for download here: 
# https://drive.google.com/drive/folders/1H7F4PuiLlcRwWtbmsJAGPEWJtLu30BN6?usp=sharing







# Step 2. Clean the temperature file

library(ncdf4)
library(plyr)
library(tidyr)
library(pbapply)
library(sf)
library(tidyverse)
library(maptools)
library(lctools)
library(raster)
library(lubridate)
library(spdep)
library(FNN)
library(patchwork)
library(dplyr)
library(readxl)

setwd("E:/Postdoc Imperial/Projects/COVID19 Greece/TutorialExcess/")


# read the files
temperature <- nc_open("temperature2015_2020_Italy.nc")
extr.tmp <- ncvar_get(temperature, varid="t2m")

# extract space and time
lon <- ncvar_get(temperature,"longitude")
lat <- ncvar_get(temperature,"latitude")
hour <- ncvar_get(temperature,"time")
# hours since 1900-01-01
hour_tr <- as.POSIXct(hour*3600, origin="1900-01-01 00:00")
hour_tr <- format(as.POSIXct(hour_tr,format='%Y-%m-%d %H:%M:%S GMT'),format='%Y-%m-%d')


dat <- data.frame(start = seq(from = 1, to = 52608, by = 24), 
                  stop = seq(from = 24, to = 52608, by = 24))

un.hour <- unique(hour_tr)
un.hour <- un.hour[order(un.hour)]
dat$date <- un.hour



# function to retrieve daily mean
DailyMean <- function(start, stop, date){
  
  tmp <- aaply(extr.tmp[,,start:stop], .margin = c(1,2), .fun = function(Y) mean(Y-273.15))
  tmp <- as.data.frame(tmp)
  
  colnames(tmp) <- lat
  rownames(tmp) <- lon
  
  mat2store <- expand.grid(lon, lat)
  colnames(mat2store) <- c("lon", "lat")
  mat2store <- cbind(mat2store, as.vector(as.matrix(tmp)))  
  
  mat2store <- as.data.frame(mat2store)
  colnames(mat2store)[3] <- "temperature"
  
  mat2store <- as.data.frame(mat2store)
  mat2store$date <- as.Date(date)
  
  mat2store <- mat2store[complete.cases(mat2store$temperature),]
  
  return(mat2store)
}


GetTemperature <- 
  pbapply(dat, 1, function(X){
    
    return(DailyMean(start = X[1], stop = X[2], date = X[3]))
    
}
) # approximately 1h



GetTemperature <- do.call(rbind, GetTemperature)
GetTemperature %>% 
  mutate(ID = group_indices(., lon, lat)) -> GetTemperature

# saveRDS(GetTemperature, file = "E:/Postdoc Imperial/Projects/COVID19 Greece/data/temperature/tmp_it_210420")
# GetTemperature <- readRDS("E:/Postdoc Imperial/Projects/COVID19 Greece/data/temperature/tmp_it_210420")




# Now we need the shp in Italy.
mun <- read_sf("ProvCM01012020_g_WGS84.shp")

# make sure shp and temperature file are in the same projection
DT_sf <- st_as_sf(GetTemperature[, c("lon", "lat")], coords = c("lon", "lat"), crs = 4326)
DT_sf <- st_transform(DT_sf, crs = st_crs(mun))
DT_sf <- st_coordinates(DT_sf)

GetTemperature <- cbind(GetTemperature, DT_sf)


# store it 
# saveRDS(GetTemperature, file = "E:/Postdoc Imperial/Projects/COVID19 Greece/data/temperature/GetTemperature_IT_210420")
# GetTemperature <- readRDS("E:/Postdoc Imperial/Projects/COVID19 Greece/data/temperature/GetTemperature_IT_210420")





# We also need to get the weekly means
GetTemperature$week <- week(GetTemperature$date)
GetTemperature$year <- year(GetTemperature$date)


# make sure its ISO weeks
EUROSTAT_ISO <- read_excel("EUROSTAT_ISO.xls")

EUROSTAT_ISO %>% 
  mutate(EURO_TIME = as.Date(format(as.POSIXct(EUROSTAT_ISO$EURO_TIME,format='%Y-%m-%d UTC'),format='%Y-%m-%d'))) %>% 
  filter(EURO_TIME < as.Date("2021-01-01")) %>% 
  filter(EURO_TIME > as.Date("2014-12-31")) %>% 
  mutate(YEAR = format(EURO_TIME, "%Y")) %>% 
  dplyr::select(EURO_TIME, CD_EURO, YEAR, EURO_LABEL) -> 
  EUROSTAT_ISO

# merge the EUROSTAT_ISO with the temperature
GetTemperature <- left_join(GetTemperature, EUROSTAT_ISO, by = c("date" = "EURO_TIME"))
GetTemperature$ID.wy <- paste0(GetTemperature$ID, GetTemperature$EURO_LABEL)

# take the mean per week
GetTemperature %>% dplyr::group_by(ID.wy) %>% 
  dplyr::mutate(weekly.mean = mean(temperature, na.rm = TRUE)) -> GetTemperature


# remove the daily temperature
GetTemperature_tmp <- GetTemperature[!duplicated(GetTemperature$ID.wy),]
GetTemperature_tmp$temperature <- NULL
names(table(GetTemperature_tmp$EURO_LABEL)) -> namtab

# Now I need to overlay it on the shp and take the mean by municipality and week
loopID <- unique(GetTemperature_tmp$EURO_LABEL)
list.loop <- list()
list.plot <- list()

mun$IDSpace <- 1:nrow(mun)

for(i in 1:length(loopID)){
  
  print(i)
  tmp <- GetTemperature_tmp %>% filter(EURO_LABEL %in% loopID[i])
  tmp_sf <- st_as_sf(tmp, coords = c("X", "Y"), crs = st_crs(mun))
  tmp_sf$X <- tmp$X
  tmp_sf$Y <- tmp$Y
  
  tmp_stjoin <- st_join(mun, tmp_sf)
  
  tmp_stjoin <- as.data.frame(tmp_stjoin)
  tmp_stjoin$geometry <- NULL
  
  # the missings are basically the same week and year, so I will impute accordingly
  tmp_stjoin$EURO_LABEL[is.na(tmp_stjoin$EURO_LABEL)] <- tmp_stjoin$EURO_LABEL[!is.na(tmp_stjoin$EURO_LABEL)][1]

  # and calculate mean temperature of points that fall in a particular municipality 
  tmp_stjoin %>% group_by(IDSpace) %>% 
    mutate(mean.temp = mean(weekly.mean, na.rm = TRUE)) %>% 
    filter(!duplicated(IDSpace)) -> tmp_stjoin
  
  tmp_stjoin <- tmp_stjoin[,c("IDSpace", "EURO_LABEL", "mean.temp")]
  tmp_stjoin$IDSpace <- as.character(tmp_stjoin$IDSpace)
  mun$IDSpace <- as.character(mun$IDSpace)
  
  list.loop[[i]] <- tmp_stjoin
}


loop.df <- do.call(rbind, list.loop)
tab2link <- as.data.frame(mun[,c("SIGLA", "IDSpace")])
tab2link$geometry <- NULL
loop.df <- left_join(loop.df, tab2link, by = c("IDSpace" = "IDSpace"))
colnames(loop.df)[1] <- "ID"


# The temperature file clean
saveRDS(loop.df, file = "TemperatureWeeklyItaly")










# Code for Figure 1

GetTemperature[GetTemperature$date == "2015-01-01",] -> tmp_points

tmp.rstr <- raster("temperature2015_2020_Italy.nc")
plot(tmp.rstr[[1]])

gplot_data <- function(x, maxpixels = 50000)  {
  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  ## Extract values
  dat <- utils::stack(as.data.frame(raster::getValues(x))) 
  names(dat) <- c('value', 'variable')
  
  dat <- dplyr::as.tbl(data.frame(coords, dat))
  
  if (!is.null(levels(x))) {
    dat <- dplyr::left_join(dat, levels(x)[[1]], 
                            by = c("value" = "ID"))
  }
  dat
}

gplot_r <- gplot_data(tmp.rstr[[1]])
gplot_r$value <- gplot_r$value -273.15 
ggplot()  + theme_light() + 
  geom_tile(data = dplyr::filter(gplot_r, !is.na(value)), aes(x = x, y = y, fill = value)) + 
  ylab("") + xlab("") + 
  scale_fill_viridis_c(name = "") +
  ggtitle("ERA5 temperature at \n2015-01-01 00:00:00") -> p1



ggplot()  + theme_light() + 
  geom_point(data = tmp_points, aes(x = X, y = Y), size = 0.1, col = "grey44") + 
  geom_sf(data = mun, fill = "NA", size = 1, col = "dodgerblue3") + 
  ylab("") + xlab("") + 
  ggtitle("NUTS3 regions and centroid of \nERA5 pixels")-> p2



loop.df %>% filter(EURO_LABEL %in% "2015-W01") %>% 
  left_join(mun, ., by = ("SIGLA" = "SIGLA")) -> tmp_mun


ggplot()  + theme_light() + 
  geom_sf(data = tmp_mun, aes(fill = mean.temp), col = "grey44") + 
  ylab("") + xlab("") + 
  scale_fill_viridis_c(name = "") + 
  ggtitle("Mean temperature during the \n1st week of 2015") -> p3

png("Fig1.png", width = 30, height = 13, units = "cm", res = 300)
p1|p2|p3
dev.off()





##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################