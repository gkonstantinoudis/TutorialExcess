## Estimating and visualising excess mortality during the COVID-19 pandemic with R-INLA and R-shiny

This repository includes all files to download and perform Bayesian hierarchical spatiotemporal models to estimate excess mortality during the 2019 COVID-19 pandemic in Italy. The 1-3 files are data wrangling files to harmonise and prepare to merge with the mortality data. They can run independently. The 4th file is the population interpolation that requires file 3 to run. The 5th file cleans the mortality data and brings all the different elements together. The 6th file runs the cross validation whereas the 7th, the analysis and extracts the number of predicted deaths had the pandemic not occurred per week, age, sex and small area. Files 8 and 9 include code that provides several different aggregations of the data, while propagating the uncertainty. The rest of the files are code for the Figures. 

Instructions on how to download the raw data files are given in the corresponding Rcode. Nevertheless all the raw files can be directly downloaded from [here](https://imperialcollegelondon.box.com/s/5di16s2ybnpfcltnfcl5en2rom5fj5vd). This folder includes:
* comuni_giornaliero_31gennaio21.csv: the file including the all-cause mortality during 2020
* ProvCM01012020_g_WGS84.* : the shpfile of the italian provinces
* POP2002_2019.csv: a file containing population counts during 2002-2019
* POP2002_2019.csv: a file containing population counts in 2020
* link_table: an .rds file containing the links between NUTS2 and NUTS3 (province) regions in italy. 