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
match_file = read.csv("match_file.csv")
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
age_needed = age[,c(4,5,6,7,8)]
age_needed = age_needed[which(age_needed$YEAR=="11"),] # only this year
colnames(age_needed) = c("State","Countynames","year","age_group","total_pop")
age_needed$Countynames = str_sub(age_needed$Countynames, end=-7)
age_needed$age_old = ifelse(age_needed$age_group>=13,1,0) # over 60 years old

population_new = aggregate(age_needed$total_pop,by=list(age_needed$State,age_needed$Countynames),FUN=sum)
colnames(population_new) = c("State","Countynames","Population")
population_clean = merge(match_file,population_new,by=c("State","Countynames"))
population_clean = population_clean[,-3]

age_needed = aggregate(age_needed$total_pop,by=list(age_needed$State,age_needed$Countynames,age_needed$age_old),FUN=sum)
age_needed = age_needed[order(age_needed$Group.1),]
age_needed = age_needed[age_needed$Group.3==1,]
age_clean = age_needed[,-3]
View(age_clean)
colnames(age_clean) = c("State","Countynames","old_pop")
age_clean = merge(match_file,age_clean,by=c("State","Countynames"))
age_clean = age_clean[,-3]

# % of essential workers
occupation <- read.csv("Occupation.csv")
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
occupation_clean$FIPS = as.numeric(occupation_clean$FIPS)

# education
education <- read.csv("education.csv")
education_2018 <- education[,c(1:3,44:47)]
colnames(education_2018) = c("FIPS","State","Countynames","lowerthan_HS","with_HS","with_BS","BS_higher")
education_2018$Countynames = as.character(education_2018$Countynames)
education_noals = education_2018[grep('County',education_2018$Countynames),]
education_noals$Countynames = str_sub(education_2018$Countynames,end=-7)
education_new = rbind(education_2018[which(education_2018$State=="AK"),],education_noals)
education_clean = education_new[order(education_new$FIPS),]

rm(education_2018)
rm(education_new)

# poverty_2018
poverty <- read.csv("poverty.csv")
poverty_2018 <- poverty[,c(1:3,11)]
colnames(poverty_2018) = c("FIPS","State","Countynames","poverty_perc")
poverty_2018$Countynames = as.character(poverty_2018$Countynames)
poverty_noals = poverty_2018[grep('County',poverty_2018$Countynames),]
poverty_noals$Countynames = str_sub(poverty_noals$Countynames,end=-7)
poverty_clean = rbind(poverty_2018[which(poverty_2018$State=="AK"),],poverty_noals)
poverty_clean = poverty_clean[order(poverty_clean$FIPS),]
rm(poverty_2018)
rm(poverty_noals)


# humidity
humid <- read.csv("humidity.csv")
humid$state = as.character(humid$state)
humid$Countynames = as.character(humid$Countynames)
humid$state = str_sub(humid$state,end=-2)
humid = humid[,c(1,2,4,6)]
colnames(humid)=c("rank","AvgHumid","Countynames","State_abbrev")
humid$State_abbrev = trimws(humid$State_abbrev)

match_file = match_file[,-1]
colnames(match_file) = c("State_abbrev","State","FIPS","Countynames")
match_file$State_abbrev = as.character(match_file$State_abbrev)
match_file$Countynames = trimws(match_file$Countynames)
humid_clean = merge(humid,match_file,by=c("State_abbrev","Countynames"))
rm(humid)

# Race
race = read.csv("cc-est2018-alldata.csv")
race$black = race$BA_FEMALE+race$BA_MALE
race = race[,c("STNAME","CTYNAME","YEAR","AGEGRP","black")]
race = race[which(race$YEAR==11),]
race_new = aggregate(race$black,by=list(race$STNAME,race$CTYNAME),FUN=sum)
colnames(race_new)=c("State","Countynames","AfricanAme_pop")
race_new$Countynames = str_sub(race_new$Countynames,end=-7)
match_file = read.csv("match_file.csv")
race_clean = merge(race_new,match_file,by=c("State","Countynames"))
race_clean = race_clean[,-4]
colnames(race_clean)[4] = "State_abbrev"
# -----------------------------------------------------------------
# Demographic Dataset --- Cleaning the whole dataset
# -----------------------------------------------------------------

demographic = merge(population_clean,age_clean,by="FIPS")
demographic = merge(demographic,education_clean,by="FIPS")
demographic = merge(demographic,poverty_clean,by="FIPS")
demographic = merge(demographic,race_clean,by="FIPS")
demographic = merge(demographic,occupation_clean,by="FIPS")
demographic = merge(demographic,humid_clean,by="FIPS")

dim(demographic)

demographic = demographic[,c(1:5,9,12:15,18,21,23,26,27)]
colnames(demographic) = c("FIPS","State","Countynames","State_abbrev","Population","Older_population",
                          "beforeHS","withHS","withBS","BShigher","Poverty_percentage","African_American_pop",
                          "Essential_job","HumidRank","AvgHumid")
demographic$older_perc = demographic$Older_population/demographic$Population
demographic$beforeHS = demographic$beforeHS/100
demographic$withHS = demographic$withHS/100
demographic$withBS = demographic$withBS/100
demographic$BShigher = demographic$BShigher/100
demographic$Poverty_percentage = demographic$Poverty_percentage/100
demographic$essential_perc = demographic$Essential_job/demographic$Population #!??
demographic$AfriAm_perc = demographic$African_American_pop/demographic$Population

# -----------------------------------------------------------------
# Export all Datasets
# -----------------------------------------------------------------
write.csv(poverty_clean,"/Users/chloehe/Desktop/COVID-19/clean_demo/poverty.csv")
write.csv(education_clean,"/Users/chloehe/Desktop/COVID-19/clean_demo/education.csv")
write.csv(occupation_clean,"/Users/chloehe/Desktop/COVID-19/clean_demo/occupation.csv")
write.csv(age_clean,"/Users/chloehe/Desktop/COVID-19/clean_demo/age.csv")
write.csv(population_clean,"/Users/chloehe/Desktop/COVID-19/clean_demo/population.csv")
write.csv(humid_clean,"/Users/chloehe/Desktop/COVID-19/clean_demo/humidity.csv")
write.csv(race_clean,"/Users/chloehe/Desktop/COVID-19/clean_demo/race.csv")

write.csv(demographic,"/Users/chloehe/Desktop/COVID-19/clean_demo/demographic_clean.csv")




