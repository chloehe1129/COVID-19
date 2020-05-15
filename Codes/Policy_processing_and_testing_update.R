
# __________________________________________________________________________________#
# ________________________________ POLICY PROCESSING _______________________________#
# __________________________________________________________________________________#

df = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") # extract the most updated cases information

df = df[,-c(1:4,8)]
df = df[,-c(4:6)]
library(data.table)
df =melt(setDT(df), id.vars = c("FIPS","Admin2","Province_State"), variable.name = "Date")
colnames(df) = c("FIPS","Countynames","State","Date","Cases")
df$Date = as.character(df$Date)
df$year = 0
df$month = 0
df$day = 0
df$year = "2020"
library(qdapRegex)
df$month = rm_between(df$Date, "X", ".", extract=TRUE)
df$day = rm_between(df$Date, ".", ".", extract=TRUE)
df$date1 = paste0(df$year,"-",df$month,"-",df$day)
df$date1 = as.Date(df$date1,format = "%Y-%m-%d")
df = df[,-c(4,6,7,8)]
cases = df 
colnames(cases) = c("FIPS","county","state","cum_case_JHU","date")


library(zoo)
library(ggplot2)
#setwd("~/Desktop/georgetown_project")
#cases = read.csv("us-states copy.txt")
SD = read.csv("~/Desktop/georgetown_project/USstatesCov19distancingpolicy.txt")
lockdown = read.csv("~/Desktop/georgetown_project/lockdown_us copy.txt")

lockdown = lockdown[lockdown$County=="",]
lockdown$Type = ifelse(lockdown$Type=="Shelter in place"|lockdown$Type=="Stay at home",1,0)
colnames(lockdown) = c("country","state","county","date","Type")
lockdown$date = as.Date(lockdown$date,format = "%m/%d/%y")
#cases$date = as.Date(cases$date,format = "%m/%d/%y")

cases$lock_down = 0
df = merge(lockdown,cases,by=c("state","date"),all=TRUE)
df = df[,-c(3:4)]
colnames(df)[3]="lock_down"
df$lock_down[is.na(df$lock_down)] <- 0


SD1 = SD[,c(4,5,8)]
colnames(SD1) = c("state","policy","date")
SD1$date1 = as.character(SD1$date)
SD1$date1 = sub("(.{4})(-*)", "\\1-\\2", SD1$date1)
SD1$date1 = sub("(.{7})(-*)", "\\1-\\2", SD1$date1)
SD1$date2 = as.Date(SD1$date1, origin = "1970-01-01")
SD_new = SD1[,c(1,2,5)]
colnames(SD_new) = c("state","policy","date")

data = merge(SD_new,df,by=c("state","date"),all=TRUE)
data = data[,-8]

rownames(data)=c(1:nrow(data))
dup_index = 0
for (i in 1:length(unique(data$FIPS))){
  dup_index = cbind(dup_index
                    ,if(sum(duplicated(data[which(data$FIPS==unique(df$FIPS)[i]),]$date))==0){0}else{
                      t(rownames(data[which(data$FIPS==unique(data$FIPS)[i]),][which(duplicated(data[which(data$FIPS==unique(data$FIPS)[i]),]$date)==TRUE),]))})
}

dup_index=dup_index[-1]
dup_index = as.numeric(dup_index)
length(dup_index)

data = data[-dup_index,]

# Create "Gather" variable: categorize gathering policy 
data$Gather = data$policy
data$Gather = as.character(data$Gather)
data[-grep('Gath',data$Gather),]$Gather = 0
#data[grep('Recom',data$Gather),]$Gather = 0
#data[grep('Gath',data$policy),]$Gather = 1
data$Gather = ifelse(data$Gather=="GathRestrict1000",1,data$Gather)
data$Gather = ifelse(data$Gather=="GathRestrict500",2,data$Gather)
data$Gather = ifelse(data$Gather=="GathRestrict250",3,data$Gather)
data$Gather = ifelse(data$Gather=="GathRestrict100",4,data$Gather)
data$Gather = ifelse(data$Gather=="GathRestrict50",5,data$Gather)
data$Gather = ifelse(data$Gather=="GathRestrict25",6,data$Gather)
data$Gather = ifelse(data$Gather=="GathRestrict10",7,data$Gather)
data$Gather = ifelse(data$Gather=="GathRestrict5",8,data$Gather)
data$Gather = ifelse(data$Gather=="GathRestrictAny",9,data$Gather)


# Align policy (LD and Gathering) with time
number_of_states = length(unique(data$state))
data$Gather0 = 0

for (i in (1:number_of_states)){
  data[data$state==unique(data$state)[i],]$Gather[data[data$state==unique(data$state)[i],]$Gather==0] <- NA
  data[data$state==unique(data$state)[i],]$Gather0 = na.locf(data[data$state==unique(data$state)[i],]$Gather,fromLast=FALSE,na.rm=FALSE)
}

data$lock_down0 = 0
for (i in (1:number_of_states)){
  data[data$state==unique(data$state)[i],]$lock_down[data[data$state==unique(data$state)[i],]$lock_down==0] <- NA
  data[data$state==unique(data$state)[i],]$lock_down0 = na.locf(data[data$state==unique(data$state)[i],]$lock_down,fromLast=FALSE,na.rm=FALSE)
}

data[which(is.na(data$Gather0)),]$Gather0 = 0
data[which(is.na(data$lock_down0)),]$lock_down0 = 0

data = data[,-c(4,8)]


# Create Other Variables
data$school_close = 0
data$school_close0 = 0
data[which(data$policy=="SchoolClose"),]$school_close = 1
for (i in (1:number_of_states)){  data[data$state==unique(data$state)[i],]$school_close[data[data$state==unique(data$state)[i],]$school_close==0] <- NA
data[data$state==unique(data$state)[i],]$school_close = na.locf(data[data$state==unique(data$state)[i],]$school_close,fromLast=FALSE,na.rm=FALSE)
}
data[which(is.na(data$school_close)),]$school_close = 0
data=data[,-9]


data$business_close = 0
data[which(data$policy=="OtherBusinessClose" |data$policy=="NEBusinessClose"),]$business_close = 1
for (i in (1:number_of_states)){  data[data$state==unique(data$state)[i],]$business_close[data[data$state==unique(data$state)[i],]$business_close==0] <- NA
data[data$state==unique(data$state)[i],]$business_close = na.locf(data[data$state==unique(data$state)[i],]$business_close,fromLast=FALSE,na.rm=FALSE)
}
data[which(is.na(data$business_close)),]$business_close = 0

data$restaurant_close = 0
data[which(data$policy=="RestaurantRestrict"),]$restaurant_close = 1
for (i in (1:number_of_states)){  data[data$state==unique(data$state)[i],]$restaurant_close[data[data$state==unique(data$state)[i],]$restaurant_close==0] <- NA
data[data$state==unique(data$state)[i],]$restaurant_close = na.locf(data[data$state==unique(data$state)[i],]$restaurant_close,fromLast=FALSE,na.rm=FALSE)
}
data[which(is.na(data$restaurant_close)),]$restaurant_close = 0

data$quarantine = 0
data[which(data$policy=="Quarantine"),]$quarantine = 1
for (i in (1:number_of_states)){  data[data$state==unique(data$state)[i],]$quarantine[data[data$state==unique(data$state)[i],]$quarantine==0] <- NA
data[data$state==unique(data$state)[i],]$quarantine = na.locf(data[data$state==unique(data$state)[i],]$quarantine,fromLast=FALSE,na.rm=FALSE)
}
data[which(is.na(data$quarantine)),]$quarantine = 0

data$curfew = 0
data[which(data$policy=="StateCurfew"),]$curfew = 1
for (i in (1:number_of_states)){  data[data$state==unique(data$state)[i],]$curfew[data[data$state==unique(data$state)[i],]$curfew==0] <- NA
data[data$state==unique(data$state)[i],]$curfew = na.locf(data[data$state==unique(data$state)[i],]$curfew,fromLast=FALSE,na.rm=FALSE)
}
data[which(is.na(data$curfew)),]$curfew = 0

data$emergdec = 0
data[which(data$policy=="EmergDec"),]$emergdec = 1
for (i in (1:number_of_states)){  data[data$state==unique(data$state)[i],]$emergdec[data[data$state==unique(data$state)[i],]$emergdec==0] <- NA
data[data$state==unique(data$state)[i],]$emergdec = na.locf(data[data$state==unique(data$state)[i],]$emergdec,fromLast=FALSE,na.rm=FALSE)
}
data[which(is.na(data$emergdec)),]$emergdec = 0

data$travel = 0
data[which(data$policy=="TravelRestrictIntra"),]$travel = 1
for (i in (1:number_of_states)){  data[data$state==unique(data$state)[i],]$travel[data[data$state==unique(data$state)[i],]$travel==0] <- NA
data[data$state==unique(data$state)[i],]$travel = na.locf(data[data$state==unique(data$state)[i],]$travel,fromLast=FALSE,na.rm=FALSE)
}
data[which(is.na(data$travel)),]$travel = 0

write.csv(data,"~/Desktop/COVID-19/clean_cases/clean_policy.csv")


# ______________________________________________________________________________________________________________#
# ________________________________ INPUT Selected Policies and TESTING DATASET _________________________________#
# ______________________________________________________________________________________________________________#

# this "varying_variable" comes from line 189 from "data_processing"
varying_variable = read.csv("~/Desktop/COVID-19/clean_cases/clean_policy.csv") # has all different measures of policies per counties
varying_variable = varying_variable[,c(2,3,5,7:16)]
colnames(varying_variable) = c("State","date","FIPS","cases","Gather","lock_down"
                               ,"school_close","business_close","restaurant_close","quarantine"
                               ,"curfew","emergdec","travel")
varying_variable$date1=as.numeric(varying_variable$date)
varying_variable$Mask = 0
varying_variable[which(varying_variable$date1>=74),]$Mask=1 # MASK policy was announced after April 3rd
varying_variable = varying_variable[,-14]

# check/get rid of duplicated dates for each county
dup = 0
for (i in 1:length(unique(varying_variable$FIPS))){
  dup = cbind(dup,if(sum(duplicated(varying_variable[which(varying_variable$FIPS==unique(varying_variable$FIPS)[i]),]$date))==0){0}else{
    t(rownames(varying_variable[which(varying_variable$FIPS==unique(varying_variable$FIPS)[i]),][which(duplicated(varying_variable[which(varying_variable$FIPS==unique(varying_variable$FIPS)[i]),]$date)==TRUE),]))})
}

# Create social distancing: if any of them is 0, then zero.
varying_variable$Social_Distancing = 0 
# index = which(sum(varying_variable[,c("Gather","school_close","business_close"
#                                ,"restaurant_close","quarantine"
#                                ,"curfer","emergdec","travel")])>=1)
# 
#for (i in 1:nrow(varying_variable)){
#  varying_variable[i,]$Social_Distancing = ifelse(sum(varying_variable[1,c(5,7:14)]==1)>=1,1,0)
#}

for (i in 1:nrow(varying_variable)){
  varying_variable[i,]$Social_Distancing = ifelse(sum(varying_variable[i,c("Gather","school_close","business_close"
                                                                           ,"restaurant_close"
                                                                           ,"curfew","emergdec","travel")]==1)>=1,1,0)}

write.csv(varying_variable,"/Users/chloehe/Desktop/COVID-19/clean_cases/clean_policies.csv")

# https://docs.google.com/spreadsheets/u/2/d/e/2PACX-1vRwAqp96T9sYYq2-i7Tj0pvTf6XVHjDSMIKBdZHXiCGGdNC0ypEU9NbngS8mxea55JuCFuua1MUeOj5/pubhtml#
testing = read.csv("/Users/chloehe/Desktop/COVID-19/us data/COVID_data/testing.csv") 
testing$Positive = as.numeric(testing$Positive)
testing$Negative = as.numeric(testing$Negative)
for (i in 1:nrow(testing)){
  testing$test[i] = sum(testing$Positive[i],testing$Negative[i],na.rm = TRUE)
}
testing = testing[order(testing$State),]
testing$pos_rate = testing$Positive/testing$test
write.csv(testing,"/Users/chloehe/Desktop/COVID-19/clean_cases/clean_testing.csv")



