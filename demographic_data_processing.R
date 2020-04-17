setwd("~/Desktop/COVID-19/us data")
# -----------------------------------------------------------------
# COVID-19 DATASET
# -----------------------------------------------------------------

# Worldwide County Level Confirmed/Death/Recovered/Active  ount 
covid_10 <- read.csv("County-level 04-10-2020.csv")
covid_us = subset(covid_10,Country_Region == "US" & is.na(FIPS)!=T)

# these two files are more like a time-line for each county (by nyc times)
df1 <- read.csv("us-counties.csv") 
df2 <- read.csv("us-states.csv")

# -----------------------------------------------------------------
# Demographic Dataset --- Cleaning individual datasets
# -----------------------------------------------------------------
setwd("~/Desktop/COVID-19/us data/Demographic")
# Population Density
population = read.csv("2019_population_county.csv") ## need population density
population_clean <- population[!population$County.Name=="Statewide Unallocated",]
colnames(population_clean) = c("FIPS","Countynames","State","population")
population_clean$Countynames = str_sub(population_clean$Countynames, end=-7)
rm(population)

# Age group
library(stringr)
library(dplyr)
age = read.csv("cc-est2018-alldata.csv")
age_needed = age[,c(4,5,7,8)]
colnames(age_needed) = c("State","Countynames","age_group","total_pop")
age_needed$Countynames = str_sub(age_needed$Countynames, end=-7)
age_needed$age_old = ifelse(age_needed$age_group>=13,1,0) # over 60 years old

population_new = aggregate(age_needed$total_pop,by=list(age_needed$State,age_needed$Countynames),FUN=sum)
colnames(population_new) = c("State","Countynames","Population")
population_clean = merge(match_file,population_new,by=c("State","Countynames"))

age_needed = aggregate(age_needed$total_pop,by=list(age_needed$State,age_needed$Countynames,age_needed$age_old),FUN=sum)
age_needed = age_needed[order(age_needed$Group.1),]
age_needed = age_needed[age_needed$Group.3==1,]
age_clean = age_needed[,-3]
View(age_clean)
colnames(age_clean) = c("State","Countynames","old_pop")
age_clean = merge(match_file,age_clean,by=c("State","Countynames"))


# % of essential workers
occupation <- read.csv("Occupation.csv")
rm(occupation)
occupation_2018 <- occupation[,c(1,2,7,26)]
colnames(occupation_2018) = c("FIPS","Countynames","description","number_of_jobs")
needed_jobs = occupation_2018[c(9:27),]$description
occupation_2018 <- occupation_2018[occupation_2018$description %in% needed_jobs, ]
occupation_2018 <- occupation_2018[grep(',',occupation_2018$Countynames),]
occupation_2018$Countynames = str_sub(occupation_2018$Countynames, end=-5)
occupation_2018$description = as.character(occupation_2018$description)
occupation_2018$description = substr(occupation_2018$description,start=4,stop=nchar(occupation_2018$description))
essential_jobs = c("Forestry, fishing, and related activities","Mining, quarrying, and oil and gas extraction",
                   "Utilities","Construction","Manufacturing","Wholesale trade","Transportation and warehousing")
occupation_2018$essential_jobs = ifelse(occupation_2018$description %in% essential_jobs,1,0)
occupation_2018$number_of_jobs = as.numeric(occupation_2018$number_of_jobs)
occupation_clean = aggregate(occupation_2018$number_of_jobs,
                             by=list(occupation_2018$FIPS,occupation_2018$essential_jobs),FUN=sum)
occupation_clean = occupation_clean[occupation_clean$Group.2==1,]
occupation_clean = occupation_clean[,-2]
colnames(occupation_clean) = c("FIPS","essential_job_numbers")
rownames(occupation_clean) = c(1:nrow(occupation_clean))
occupation_clean$FIPS = as.character(occupation_clean$FIPS)
occupation_clean[c(1:340),]$FIPS = substr(occupation_clean[c(1:340),]$FIPS,3,6)
occupation_clean[c(341:nrow(occupation_clean)),]$FIPS = substr(occupation_clean[c(341:nrow(occupation_clean)),]$FIPS,2,7)

# education
education <- read.csv("education.csv")
education_2018 <- education[,c(1:3,44:47)]
colnames(education_2018) = c("FIPS","State","Countynames","lowerthan_HS","with_HS","with_BS","BS_higher")
education_2018 = education_2018[grep('County',education_2018$Countynames),]
education_2018$Countynames = str_sub(education_2018$Countynames,end=-7)
education_clean = education_2018
rm(education_2018)

# poverty_2018
poverty <- read.csv("poverty.csv")
poverty_2018 <- poverty[,c(1:3,11)]
colnames(poverty_2018) = c("FIPS","State","Countynames","poverty_perc")
poverty_2018 = poverty_2018[grep('County',poverty_2018$Countynames),]
poverty_2018$Countynames = str_sub(poverty_2018$Countynames,end=-7)
poverty_clean = poverty_2018
rm(poverty_2018)
rownames(population_clean) = c(1:nrow(population_clean))
rownames(age_clean) = c(1:nrow(age_clean))

# humidity
url <- 'http://www.usa.com/rank/us--average-humidity--county-rank.htm?yr=9000&dis=&wist=&plow=&phigh=#'

# -----------------------------------------------------------------
# Demographic Dataset --- Cleaning the whole dataset
# -----------------------------------------------------------------

demographic = merge(population_clean,age_clean,by="FIPS")
demographic = merge(demographic,education_clean,by="FIPS")
demographic = merge(demographic,poverty_clean,by="FIPS")
demographic = demographic[,c(1:5,9,12:15,18)]
colnames(demographic) = c("FIPS","Countynames","State","State_abbrev","Population","Older_population",
                          "beforeHS","withHS","withBS","BShigher","Poverty_percentage")
demographic$older_perc = demographic$Older_population/demographic$Population
demographic$beforeHS = demographic$beforeHS/100
demographic$withHS = demographic$withHS/100
demographic$withBS = demographic$withBS/100
demographic$BShigher = demographic$BShigher/100
demographic$Poverty_percentage = demographic$Poverty_percentage/100


demographic = merge(demographic,occupation_clean,by="FIPS")
dim(demographic)

# -----------------------------------------------------------------
# Export all Datasets
# -----------------------------------------------------------------
write.csv(poverty_clean,"/Users/chloehe/Desktop/COVID-19/clean_demo/poverty.csv")
write.csv(education_clean,"/Users/chloehe/Desktop/COVID-19/clean_demo/education.csv")
write.csv(occupation_clean,"/Users/chloehe/Desktop/COVID-19/clean_demo/occupation.csv")
write.csv(age_clean,"/Users/chloehe/Desktop/COVID-19/clean_demo/age.csv")
write.csv(population_clean,"/Users/chloehe/Desktop/COVID-19/clean_demo/population.csv")

