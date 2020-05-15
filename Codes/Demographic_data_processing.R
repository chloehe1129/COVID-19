# -----------------------------------------------------------------
# Demographic Dataset --- Cleaning individual datasets
# -----------------------------------------------------------------
setwd("~/Desktop/COVID-19/us data/Demographic")
match_file = read.csv("match_file.csv")

# --------------------------------- Population, Race, Age Group ---------------------------------
library(stringr)
library(dplyr)
data = read.csv("cc-est2018-alldata.csv")
data_needed = data[,c(4,5,6,7,8,25,26,57,58)]
data_needed = data_needed[which(data_needed$YEAR=="11"),] # only this year
colnames(data_needed) = c("State","Countynames","year","age_group","total_pop","black_male","black_female"
                         ,"hispanic_male","hispanic_female")
data_needed$Countynames = ifelse(grepl("County",data_needed$Countynames),str_sub(data_needed$Countynames, end=-7)
                                ,as.character(data_needed$Countynames))

# county-level-population
population_new = aggregate(data_needed$total_pop,by=list(data_needed$State,data_needed$Countynames),FUN=sum)
colnames(population_new) = c("State","Countynames","Population")
population_clean = merge(match_file,population_new,by=c("State","Countynames"))
population_clean = population_clean[,-3]

# state-level-population
population_state = aggregate(data_needed$total_pop,by=list(data_needed$State),FUN=sum)
colnames(population_state) = c("State","Population")

# county-level-race
race_clean = aggregate(list(data_needed$total_pop, data_needed$black_male,data_needed$black_female
                            ,data_needed$hispanic_male, data_needed$hispanic_female)
                       , by = list(data_needed$State,data_needed$Countynames), FUN=sum)
colnames(race_clean) = c("State","Countynames","total_pop","black_male","black_female"
                         ,"hispanic_male","hispanic_female")

race_clean = race_clean[order(race_clean$State,race_clean$Countynames),]
race_clean = merge(match_file,race_clean,by=c("State","Countynames"))
race_clean = race_clean[,-3]

# state-level-race
race_clean_state = aggregate(list(data_needed$total_pop, data_needed$black_male,data_needed$black_female
                                  ,data_needed$hispanic_male, data_needed$hispanic_female)
                             ,by=list(data_needed$State),FUN=sum)
colnames(race_clean_state) = c("State","total_pop","black_male","black_female"
                               ,"hispanic_male","hispanic_female")


# Create age group
data_needed$age_group_new = ifelse(data_needed$age_group %in% c(4,5,6,7,8,9),1
                                    ,ifelse(data_needed$age_group %in% c(10,11,12,13),2
                                            ,ifelse(data_needed$age_group %in% c(13,14,15,16,17,18),3,0)))
# county-level-age-group 
age_needed = aggregate(list(data_needed$total_pop)
                       ,by = list(data_needed$State,data_needed$Countynames,data_needed$age_group_new), FUN=sum)
colnames(age_needed) = c("State","Countynames","age_group","total_pop")
age_needed = age_needed[order(age_needed$State,age_needed$Countynames,age_needed$age_group),]
age_needed$young_perc=0
age_needed$median_perc=0
age_needed$old_perc=0
for (i in 1:length(unique(age_needed$Countynames))){
  age_needed[which(age_needed$Countynames==unique(age_needed$Countynames)[i]),]$young_perc = age_needed[which(age_needed$Countynames==unique(age_needed$Countynames)[i]),]$total_pop[2]/sum(age_needed[which(age_needed$Countynames==unique(age_needed$Countynames)[i]),]$total_pop)
  age_needed[which(age_needed$Countynames==unique(age_needed$Countynames)[i]),]$median_perc = age_needed[which(age_needed$Countynames==unique(age_needed$Countynames)[i]),]$total_pop[3]/sum(age_needed[which(age_needed$Countynames==unique(age_needed$Countynames)[i]),]$total_pop)
  age_needed[which(age_needed$Countynames==unique(age_needed$Countynames)[i]),]$old_perc = age_needed[which(age_needed$Countynames==unique(age_needed$Countynames)[i]),]$total_pop[4]/sum(age_needed[which(age_needed$Countynames==unique(age_needed$Countynames)[i]),]$total_pop)
}
age_needed = age_needed[,c(1,2,5,6,7)]
age_needed = age_needed[!duplicated(age_needed),]

age_clean = merge(match_file,age_needed,by=c("State","Countynames"))
age_clean = age_clean[,-3]

# state- level-age-group 
age_needed_state = aggregate(list(age_needed$young_perc,age_needed$median_perc,age_needed$old_perc)
                             ,by=list(age_needed$State),FUN=mean)
colnames(age_needed_state) = c("State","young_perc","median_perc","old_perc")

# age_needed = aggregate(age_needed$total_pop,by=list(age_needed$State,age_needed$Countynames,age_needed$age_old),FUN=sum)
# age_needed = age_needed[order(age_needed$Group.1),]
# age_needed = age_needed[age_needed$Group.3==1,]
# age_clean = age_needed[,-3]
# View(age_clean)
# colnames(age_clean) = c("State","Countynames","old_pop")


# # % of essential workers
# occupation <- read.csv("Occupation.csv")
# occupation_2018 <- occupation[,c(1,2,7,26)]
# colnames(occupation_2018) = c("FIPS","Countynames","description","number_of_jobs")
# needed_jobs = occupation_2018[c(9:27),]$description
# occupation_2018 <- occupation_2018[occupation_2018$description %in% needed_jobs, ]
# occupation_2018 <- occupation_2018[grep(',',occupation_2018$Countynames),]
# occupation_2018$Countynames = str_sub(occupation_2018$Countynames, end=-5)
# occupation_2018$description = as.character(occupation_2018$description)
# occupation_2018$description = substr(occupation_2018$description,start=4,stop=nchar(occupation_2018$description))
# essential_jobs = c("Forestry, fishing, and related activities","Mining, quarrying, and oil and gas extraction",
#                    "Utilities","Construction","Manufacturing","Wholesale trade","Transportation and warehousing")
# occupation_2018$essential_jobs = ifelse(occupation_2018$description %in% essential_jobs,1,0)
# occupation_2018$number_of_jobs = as.numeric(occupation_2018$number_of_jobs)
# occupation_clean = aggregate(occupation_2018$number_of_jobs,
#                              by=list(occupation_2018$FIPS,occupation_2018$essential_jobs),FUN=sum)
# occupation_clean = occupation_clean[occupation_clean$Group.2==1,]
# occupation_clean = occupation_clean[,-2]
# colnames(occupation_clean) = c("FIPS","essential_job_numbers")
# rownames(occupation_clean) = c(1:nrow(occupation_clean))
# occupation_clean$FIPS = as.character(occupation_clean$FIPS)
# occupation_clean[c(1:340),]$FIPS = substr(occupation_clean[c(1:340),]$FIPS,3,6)
# occupation_clean[c(341:nrow(occupation_clean)),]$FIPS = substr(occupation_clean[c(341:nrow(occupation_clean)),]$FIPS,2,7)
# occupation_clean$FIPS = as.numeric(occupation_clean$FIPS)

# --------------------------------- Education ---------------------------------
education <- read.csv("education.csv")
education_2018 <- education[,c(1:3,44:47)] # only 2018 year
colnames(education_2018) = c("FIPS","State","Countynames","lowerthan_HS","with_HS","with_BS","BS_higher")
education_2018$Countynames = as.character(education_2018$Countynames)
education_2018$Countynames = ifelse(grepl("County",education_2018$Countynames),str_sub(education_2018$Countynames, end=-7)
       ,as.character(education_2018$Countynames))

# county-level-education
education_clean = education_2018[order(education_2018$FIPS),]

# state-level-education
education_clean_state = aggregate(list(education_clean$lowerthan_HS,education_clean$with_HS
                                       ,education_clean$with_BS,education_clean$BS_higher),
                                  by=list(education_clean$State),FUN=mean,na.rm=TRUE)

colnames(education_clean_state)=c("State","lowerthan_HS","with_HS","with_BS","BS_higher")

rm(education_2018)

# --------------------------------- Poverty and HH income ---------------------------------
poverty <- read.csv("poverty.csv")
poverty_2018 <- poverty[,c(1:3,11,26)]
colnames(poverty_2018) = c("FIPS","State","Countynames","poverty_perc","Median_HH_income")
poverty_2018$Countynames = as.character(poverty_2018$Countynames)

poverty_2018$Countynames = ifelse(grepl("County",poverty_2018$Countynames),str_sub(poverty_2018$Countynames, end=-7)
       ,as.character(poverty_2018$Countynames))
poverty_clean = poverty_2018

# county-level poverty
poverty_clean$Median_HH_income = as.numeric(gsub(",","",poverty_clean$Median_HH_income))
poverty_clean = poverty_clean[order(poverty_clean$FIPS),]

# state-level poverty
poverty_clean_state = aggregate(list(poverty_clean$poverty_perc,poverty_clean$Median_HH_income)
                                ,by=list(poverty_clean$State),FUN=mean,na.rm=TRUE)
colnames(poverty_clean_state) = c("State","poverty_perc","Median_HH_income")

rm(poverty_2018)



# # humidity
# humid <- read.csv("humidity.csv")
# humid$state = as.character(humid$state)
# humid$Countynames = as.character(humid$Countynames)
# humid$state = str_sub(humid$state,end=-2)
# humid = humid[,c(1,2,4,6)]
# colnames(humid)=c("rank","AvgHumid","Countynames","State_abbrev")
# humid$State_abbrev = trimws(humid$State_abbrev)
# 
# match_file = match_file[,-1]
# colnames(match_file) = c("State_abbrev","State","FIPS","Countynames")
# match_file$State_abbrev = as.character(match_file$State_abbrev)
# match_file$Countynames = trimws(match_file$Countynames)
# humid_clean = merge(humid,match_file,by=c("State_abbrev","Countynames"))
# rm(humid)

# # Race
# race = read.csv("cc-est2018-alldata.csv")
# race$black = race$BA_FEMALE+race$BA_MALE
# race = race[,c("STNAME","CTYNAME","YEAR","AGEGRP","black")]
# race = race[which(race$YEAR==11),]
# race_new = aggregate(race$black,by=list(race$STNAME,race$CTYNAME),FUN=sum)
# colnames(race_new)=c("State","Countynames","AfricanAme_pop")
# race_new$Countynames = str_sub(race_new$Countynames,end=-7)
# match_file = read.csv("match_file.csv")
# race_clean = merge(race_new,match_file,by=c("State","Countynames"))
# race_clean = race_clean[,-4]
# colnames(race_clean)[4] = "State_abbrev"

# -----------------------------------------------------------------
# County-level Demographic Dataset --- Cleaning the whole dataset
# -----------------------------------------------------------------

demographic = merge(age_clean,education_clean[,c(1,4:7)],by="FIPS")
demographic = merge(demographic,poverty_clean[,c(1,4,5)],by="FIPS")
demographic = merge(demographic,population_clean[,c(4,5)],by="FIPS")
demographic = merge(demographic,race_clean[,c(4,6:9)],by="FIPS")

demographic$black = demographic$black_male+demographic$black_female
demographic$latino = demographic$hispanic_male+demographic$hispanic_female
demographic$black_perc = round(demographic$black/demographic$Population,3)
demographic$latino_perc = round(demographic$latino/demographic$Population,3)
demographic$lowerthan_HS = demographic$lowerthan_HS/100
demographic$with_HS = demographic$with_HS/100
demographic$with_BS = demographic$with_BS/100
demographic$BS_higher = demographic$BS_higher/100
demographic$poverty_perc = demographic$poverty_perc/100


# demographic$young_perc = ifelse(demographic$age_group==1,demographic$age_pop/demographic$population,0)
# demographic$median_perc = ifelse(demographic$age_group==2,demographic$age_pop/demographic$population,0)
# demographic$old_perc = ifelse(demographic$age_group==3,demographic$age_pop/demographic$population,0)
# 
# # create age percentage three groups
# for (i in 1:length(unique(demographic$FIPS))){
#   demographic[which(demographic$FIPS==unique(demographic$FIPS)[i]),]$young_perc =rep(demographic[which(demographic$FIPS==unique(demographic$FIPS)[i]&demographic$age_group==1),]$young_perc,3)
#   demographic[which(demographic$FIPS==unique(demographic$FIPS)[i]),]$median_perc =rep(demographic[which(demographic$FIPS==unique(demographic$FIPS)[i]&demographic$age_group==2),]$median_perc,3)
#   demographic[which(demographic$FIPS==unique(demographic$FIPS)[i]),]$old_perc =rep(demographic[which(demographic$FIPS==unique(demographic$FIPS)[i]&demographic$age_group==3),]$old_perc,3)
# }

dim(demographic)
demographic = demographic[,-c(15:20)]

# add density
area = read.csv("~/Desktop/COVID-19/us data/Demographic/area.csv")
area = area[-which(area$Countynames=="#VALUE!"),]
colnames(area)=c("Countynames","FIPS","area")
demographic_clean = merge(demographic,area,by="FIPS")
demographic_clean$density = demographic_clean$Population/demographic_clean$area

# add temperature and humidity
temp = read.csv("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/temp_seasonal_county.csv")
temp = temp[which(temp$year=="2016"),]
temp$summer_tmmx = (temp$summer_tmmx-273.15) * (9/5) + 32 # to fahrenheit
temp$winter_tmmx = (temp$winter_tmmx-273.15) * (9/5) + 32 # to fahrenheit
temp = temp[,-2]
colnames(temp)[1] = "FIPS"

demographic = merge(temp,demographic_clean,by="FIPS")

# -----------------------------------------------------------------
# Export county demographic dataset
# -----------------------------------------------------------------
# write.csv(poverty_clean,"/Users/chloehe/Desktop/COVID-19/clean_demo/poverty.csv")
# write.csv(education_clean,"/Users/chloehe/Desktop/COVID-19/clean_demo/education.csv")
# write.csv(occupation_clean,"/Users/chloehe/Desktop/COVID-19/clean_demo/occupation.csv")
# write.csv(age_clean,"/Users/chloehe/Desktop/COVID-19/clean_demo/age.csv")
# write.csv(population_clean,"/Users/chloehe/Desktop/COVID-19/clean_demo/population.csv")
# write.csv(humid_clean,"/Users/chloehe/Desktop/COVID-19/clean_demo/humidity.csv")
# write.csv(race_clean,"/Users/chloehe/Desktop/COVID-19/clean_demo/race.csv")

write.csv(demographic,"/Users/chloehe/Desktop/COVID-19/clean_demo/demographic_clean.csv")

# -----------------------------------------------------------------
# State-level Demographic Dataset --- Cleaning the whole dataset
# -----------------------------------------------------------------
colnames(poverty_clean_state) = c("Code","poverty_perc" ,"Median_HH_income")
colnames(education_clean_state) = c("Code","lowerthan_HS","with_HS","with_BS" ,"BS_higher")
match = match_file[,c("State","Code")][!duplicated(match_file[,c("State","Code")]), ]

poverty_clean_state = merge(poverty_clean_state,match,by="Code")
education_clean_state = merge(education_clean_state,match,by="Code")

demographic_state = merge(population_state,age_needed_state,by="State")
demographic_state = merge(demographic_state,poverty_clean_state,by="State")
demographic_state = merge(demographic_state,education_clean_state,by="State")
demographic_state = merge(demographic_state,race_clean_state,by="State")

demographic_state$black = demographic_state$black_male+demographic_state$black_female
demographic_state$latino = demographic_state$hispanic_male+demographic_state$hispanic_female
demographic_state$black_perc = round(demographic_state$black/demographic_state$Population,3)
demographic_state$latino_perc = round(demographic_state$latino/demographic_state$Population,3)
demographic_state$lowerthan_HS = demographic_state$lowerthan_HS/100
demographic_state$with_HS = demographic_state$with_HS/100
demographic_state$with_BS = demographic_state$with_BS/100
demographic_state$BS_higher = demographic_state$BS_higher/100
demographic_state$poverty_perc = demographic_state$poverty_perc/100

demographic_state = demographic_state[,c("State","Population","poverty_perc" ,"Median_HH_income"
                                         ,"beforeHS","withHS" , "withBS" ,"BShigher"
                                         ,"black_perc" ,"latino_perc","young_perc","median_perc" ,"old_perc")]
demographic_state = demographic_state[,c(1:5,7:8,10:13,21:22)]

# add density
state_area = merge(match_file,area,by="FIPS")
state_area = aggregate(state_area$area,by=list(state_area$State),FUN=sum)
colnames(state_area) = c("State","area")
demographic_clean_state = merge(demographic_state,state_area,by="State")
demographic_clean_state$density = demographic_clean_state$Population/demographic_clean_state$area

# add temperature and humidity
state_temp = merge(match_file,temp,by="FIPS")
state_temp = aggregate(list(state_temp$summer_tmmx,state_temp$winter_tmmx
                            ,state_temp$summer_rmax,state_temp$winter_rmax),by=list(state_temp$State),FUN=mean)
colnames(state_temp) = c("State","summer_tmmx","winter_tmmx","summer_rmax","winter_rmax")
demographic_state = merge(state_temp,demographic_clean_state,by="State")

write.csv(demographic_state,"/Users/chloehe/Desktop/COVID-19/clean_demo/demographic_clean_state.csv")


