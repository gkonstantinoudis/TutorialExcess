


# Created 07.10.2021


#---------------------------------------------------------------------------------


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
  target = "download_e5_single.nc"
)

file <- wf_request(user = "52967",
                   request = request,
                   transfer = TRUE,
                   path = "~",
                   verbose = TRUE)