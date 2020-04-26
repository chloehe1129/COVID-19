# this "varying_variable" comes from line 189 from "data_processing"
varying_variable = read.csv("~/Desktop/COVID-19/clean_cases/clean_policy.csv") # has all different measures of policies per counties
varying_variable = varying_variable[,c(2,3,5,7,9)]
colnames(varying_variable) = c("State","date","FIPS","cases","Social_Distancing")
varying_variable$date1=as.numeric(varying_variable$date)
varying_variable$Mask = 0
varying_variable[which(varying_variable$date1>=74),]$Mask=1 # MASK policy was announced after April 3rd
varying_variable = varying_variable[,-6]
write.csv(varying_variable,"/Users/chloehe/Desktop/COVID-19/clean_cases/clean__SD_Mask.csv")

# https://docs.google.com/spreadsheets/u/2/d/e/2PACX-1vRwAqp96T9sYYq2-i7Tj0pvTf6XVHjDSMIKBdZHXiCGGdNC0ypEU9NbngS8mxea55JuCFuua1MUeOj5/pubhtml#
testing = read.csv("/Users/chloehe/Desktop/COVID-19/us data/COVID_data/testing.csv") 
testing = testing[,c(1:4)]
for (i in 1:nrow(testing)){
  testing$test[i] = sum(testing$Positive[i],testing$Negative[i],na.rm = TRUE)
}
testing = testing[,-c(3:4)]
testing = testing[order(testing$State),]
write.csv(testing,"/Users/chloehe/Desktop/COVID-19/clean_cases/clean_testing.csv")
