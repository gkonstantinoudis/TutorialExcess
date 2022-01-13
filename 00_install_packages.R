# Run this script first in order to install all the required packages


pkgs <- c("dplyr", "ecmwfr", "FNN", "ggplot2", "grid", "lctools", "lubridate", 
  "maptools", "ncdf4", "patchwork", "pbapply", "plotly", "plyr", 
  "raster", "RColorBrewer", "readr", "reshape2", "rgdal", "sf", "sn", 
  "sp", "spacetime", "spdep", "stringr", "tidyr", "tidyverse", 
  "timeDate", "viridis", "xtable", "xts", "data.table", "tibble")
install.packages(pkgs, dep = TRUE)

# Install INLA (off CRAN)
install.packages("INLA", repos = c(getOption("repos"), 
  INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE)
