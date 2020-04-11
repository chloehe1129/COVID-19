setwd("~/Desktop/COVID-19/us data")
# -----------------------------------------------------------------
# COVID-19 DATASET
# -----------------------------------------------------------------

# Worldwide County Level Confirmed/Death/Recovered/Active  ount 
covid_10 <- read.csv("County-level 04-10-2020.csv")
covid_us = subset(covid_10,Country_Region == "US" & is.na(FIPS)!=T)

# these two files are more like a time-line for each county (by nyc times)
#df1 <- read.csv("us-counties.csv") 
#df2 <- read.csv("us-states.csv")

# -----------------------------------------------------------------
# Demographic Dataset
# -----------------------------------------------------------------
setwd("~/Desktop/COVID-19/us data/Demographic")
# Population Density
population = read.csv("2019_population_county.csv") ## need population density
dim(population)

# % of essential workers
employment <- read.csv("employment.csv") ## need 2020 unemployment rate
dim(employment)
occupation <- read.csv("Occupation.csv")
occupation_2018 <- occupation[,-c(3:6,8)]
dim(occupation)

# weather

# education
education <- read.csv("education.csv")
education_2018 <- education[,c(1:3,44:47)]
dim(education_2018)

# poverty_2018
poverty <- read.csv("poverty.csv")
poverty_2018 <- poverty[,c(1:3,11)]
dim(poverty_2018)


# covid related covariates!
# 1. time between test and test results
# 2. adherence rate



