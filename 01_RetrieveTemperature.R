


# Created 07.10.2021

# Clean and download temperature


#---------------------------------------------------------------------------------

# Step 1. Download temperature data from ERA5

# and create a new directory to store the output
if(!dir.exists("Output"))
  dir.create("Output/")

# load packages
library(ecmwfr)

# You need to create an account here https://cds.climate.copernicus.eu/cdsapp#!/home, 
# agree with the terms here: https://cds.climate.copernicus.eu/cdsapp/#!/terms/licence-to-use-copernicus-products,
# log in and once you are ok and logged in, click on your name on the top right next to logout
# and retrieve the information about the API key.

cds.user <- "your_CDS_key" # Insert your CDS user here
cds.key <- "your_CDS_API_KEY_here" #"Insert_your_CDS_API_KEY_here"

# Set up the API and UID
wf_set_key(user = cds.user, key = cds.key, service = "cds")

if(is.null(cds.user) | is.null(cds.key)) {
  print("You need to create an account here https://cds.climate.copernicus.eu/cdsapp#!/home, and once you are ok and logged in, click on your name on the top right next to logout and retrieve the information about the API key.")
}


request <- list(
  dataset_short_name = "reanalysis-era5-land",
  product_type   = "reanalysis",
  format = "netcdf",
  variable = "2m_temperature",
  date = "2015-01-01/2021-01-03", # this is to match the ISO weeks
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", 
           "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", 
           "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  # area is specified as N, W, S, E
  area = c(48, 6, 34, 20),
  target = "temperature2015_2020_Italy.nc"
)

if(!file.exists("Output/temperature2015_2020_Italy.nc")) {
  file <- wf_request(user = cds.user,
                   request = request,
                   transfer = TRUE,
                   path = "Output/",
                   time_out = 3600*24,
                   verbose = TRUE)
}

# and you will get a temperature_Italy.nc file on your working directory. 



# Alternatively one can use this link https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=overview
# select download data, and make the following selection: Temperature: 2m temperature, Years: 2015-2020, Months: Select all, Days: All, Time: Select all, 
# and for the geographical area, select sub-region extraction and use 48, 6, 34, 20 specified as N, W, S, E. Store this file on
# a new folder called "Output" in your working directory as temperature_Italy.nc.



# The temperature_Italy.nc file is also provided for download here: 
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
library(stringr)
library(data.table)

# read the files
temperature <- nc_open("Output/temperature2015_2020_Italy.nc")
extr.tmp <- ncvar_get(temperature, varid="t2m")

# extract space and time
lon <- ncvar_get(temperature,"longitude")
lat <- ncvar_get(temperature,"latitude")
hour <- ncvar_get(temperature,"time")
# hours since 1900-01-01
hour_tr <- as.POSIXct(hour*3600, origin="1900-01-01 00:00")
hour_tr <- format(as.POSIXct(hour_tr,format='%Y-%m-%d %H:%M:%S GMT'),format='%Y-%m-%d', tz = "Europe/London")

dat <- data.frame(start = seq(from = 1, to = length(hour_tr), by = 24), 
                  stop = seq(from = 24, to = length(hour_tr), by = 24))

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


# Now we need the shp in Italy.
mun <- read_sf("data/ProvCM01012020_g_WGS84.shp")

# make sure shp and temperature file are in the same projection
DT_sf <- st_as_sf(GetTemperature[, c("lon", "lat")], coords = c("lon", "lat"), crs = 4326)
DT_sf <- st_transform(DT_sf, crs = st_crs(mun))
DT_sf <- st_coordinates(DT_sf)

GetTemperature <- cbind(GetTemperature, DT_sf)


# We also need to get the weekly means
GetTemperature$week <- week(GetTemperature$date)
GetTemperature$year <- year(GetTemperature$date)


# ISO weeks file
EUROSTAT_ISO <- data.frame(
  EURO_TIME = seq(as.Date("2014-12-29"), as.Date("2021-01-03"), by="days")
)

EUROSTAT_ISO %>% mutate(num.week = lubridate::isoweek(EURO_TIME), 
                        YEAR_ISO = lubridate::isoyear(EURO_TIME), 
                        YEAR = year(EURO_TIME)) %>% 
  mutate(CD_EURO = paste0("W", str_pad(num.week, 2, pad = "0")), 
         EURO_LABEL = paste(YEAR_ISO, CD_EURO, sep = "-")) %>% 
  dplyr::select(EURO_TIME, CD_EURO, YEAR, EURO_LABEL) -> EUROSTAT_ISO

# store it because is needed for the other rfiles too.
saveRDS(EUROSTAT_ISO, file = "Output/EUROSTAT_ISO")



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

mun$IDSpace <- 1:nrow(mun)
mun$IDSpace <- as.character(mun$IDSpace)

# Work on data.table to speed up the filter() computation (line 240)
GetTemperature_tmp <- as.data.table(GetTemperature_tmp)
  
pblapply(1:length(loopID), function(X){
  
  i <- X
  tmp <- GetTemperature_tmp[EURO_LABEL == loopID[i]]
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
  
  return(tmp_stjoin)
  }
) -> list.loop


loop.df <- do.call(rbind, list.loop)
tab2link <- as.data.frame(mun[,c("SIGLA", "IDSpace")])
tab2link$geometry <- NULL
loop.df <- left_join(loop.df, tab2link, by = c("IDSpace" = "IDSpace"))
colnames(loop.df)[1] <- "ID"



# The temperature file clean
saveRDS(loop.df, file = "Output/TemperatureWeeklyItaly")







# Code for Figure 2

GetTemperature[GetTemperature$date == "2015-01-01",] -> tmp_points

tmp.rstr <- raster("Output/temperature2015_2020_Italy.nc")

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
  ggtitle("A. ERA5 temperature at \n2015-01-01 00:00:00") + 
  theme(text = element_text(size = 6),  
        legend.key.height = unit(0.3, 'cm'),
        legend.key.width = unit(0.2, 'cm'), 
        plot.margin = margin(0, 0, 0, 0), 
        legend.margin=margin(0,0,0,0))  -> p1


tmp_points2 <- tmp_points[sample(size = 2000, x = 1:nrow(tmp_points)),] 
ggplot()  + theme_light() + 
  geom_point(data = tmp_points2, aes(x = X, y = Y), size = 0.01, col = "grey44") + 
  geom_sf(data = mun, fill = "NA", size = 0.4, col = "dodgerblue3") + 
  ylab("") + xlab("") + 
  ggtitle("B. NUTS3 regions and \ncentroid of ERA5 pixels") + 
  theme(text = element_text(size = 6), 
        plot.margin = margin(0, 0, 0, 0))-> p2



loop.df %>% filter(EURO_LABEL %in% "2015-W01") %>% 
  left_join(mun, ., by = ("SIGLA" = "SIGLA")) -> tmp_mun


ggplot()  + theme_light() + 
  geom_sf(data = tmp_mun, aes(fill = mean.temp), col = "grey44", size = 0.4) + 
  ylab("") + xlab("") + 
  scale_fill_viridis_c(name = "") + 
  ggtitle("C. Mean temperature during the \n1st week of 2015") + 
  theme(text = element_text(size = 6),  
        legend.key.height = unit(0.3, 'cm'),
        legend.key.width = unit(0.2, 'cm'), 
        plot.margin = margin(0, 0, 0, 0), 
        legend.margin=margin(0,0,0,0)) -> p3

png("Output/ERAPOINTS.png", width = 17, height = 8, units = "cm", res = 300)
p1|p2|p3
dev.off()




##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
