## Estimating and visualising excess mortality during the COVID-19 pandemic with R-INLA and R-shiny

This repository includes all files to download and perform Bayesian hierarchical spatiotemporal models to estimate excess mortality during the 2019 COVID-19 pandemic in Italy. The starting with 01 files are data wrangling files to harmonise and prepare to merge with the mortality data. They can run independently. The 02 file is the population interpolation that assumes that 01_RetrivePopulation.R ran. The 03 file cleans the mortality data and brings all the different elements together. The 04 files run the cross validation and the analysis. They can run independently. The 05 files include code that provides several different aggregations of the data, while propagating the uncertainty. The 06 files are code for the Figures 3-5. 

Instructions on how to download the raw data files are given in the corresponding Rcode. Nevertheless all the raw files together with the output of the code can be directly downloaded from [here](https://imperialcollegelondon.box.com/s/5di16s2ybnpfcltnfcl5en2rom5fj5vd). 

The data folder includes:
* comuni_giornaliero_31gennaio21.csv: a file including the all-cause mortality during 2020
* ProvCM01012020_g_WGS84.* : a shapefile of the italian provinces
* POP2002_2019.csv: a file containing population counts during 2002-2019
* POP2002_2019.csv: a file containing population counts in 2020
* link_table: an .rds file containing the links between NUTS2 and NUTS3 (province) regions in italy. 

The output folder includes:
* temperature2015_2020.nc: the raw temperature file downloaded from ERA5
* yearCV_* : the cross validation results
* res_* : the model based results 
* TemperatureWeekleItaly: the clean temperature file
* poisson_samples_all: the output of the posterior samples from a poisson
* Italy.R: the different aggregations used
* pop_weekly: the weekly population file, after performing the linear interpolation
* pop15_20_final: the clean population file
* findata: the clean final data file
* holiday_df: the file including the national holidays
* EUROSTAT_ISO: the iso weeks during 2015-2020
* The 4 manuscript figures: PopulationPlot.png, ERAPOINTS.png, PCpriors.png, SpatiotemporalRegions.png and PosteriorProb.png

Download the folders, unzip and put it in your working directory. You can skip download the Output file, as all the files there can be downloaded following the instruction of the Rfiles. 
