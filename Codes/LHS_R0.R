df = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

df = df[,-c(1:4,8)]
df = df[,-c(4:6)]

df =melt(setDT(df), id.vars = c("FIPS","Admin2","Province_State"), variable.name = "Date")
colnames(df) = c("FIPS","Countynames","State","Date","Test")
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
county_case = df
colnames(county_case) = c("FIPS","county","state","case","date")

county_case <- county_case[order(county_case$FIPS),] # order based on fips number 
#county_case[which(county_case$county=="New York City"),]$fips = 36061
county_case$county = as.character(county_case$county)
county_case = subset(county_case,county_case$county!="")
county_case = subset(county_case,county_case$county!="Unassigned")
county_case$date = as.factor(county_case$date)
county_case$state = as.character(county_case$state)
county_case = subset(county_case,county_case$FIPS!="NA")
county_case = county_case[-which(county_case$case==0),]
length(unique(county_case$FIPS))

# Set max generation interval as first day that captures >99% of the density
library(R0)

# serial interval estimation 
Li_shape <- 7.5
Li_scale <- 3.4
Li_stop <- ceiling(qweibull(0.99, Li_shape, Li_scale)) # Set max generation interval as first day that captures >99% of the density

Du_shape = 3.96
Du_scale = 4.75
Du_stop = ceiling(qnorm(0.99, Du_shape, Du_scale))

Hiroshi_shape <- log(4.7)
Hiroshi_scale <- log(2.9)
Hiroshi_stop <- ceiling(qlnorm(0.99, Hiroshi_shape, Hiroshi_scale)) # Set max generation interval as first day that captures >99% of the density

Hiroshi2_shape <- 4.8
Hiroshi2_scale <- 2.3
Hiroshi2_stop <- ceiling(qweibull(0.99, shape=Hiroshi2_shape, scale=Hiroshi2_scale)) 


## Write function to pull value of serial interval distribution
func.SI_pull <- function(value, serial_int){
  if(serial_int == "Li"){
    return(dweibull(value, shape=Li_shape, scale=Li_scale))
  } else if(serial_int == "Hiroshi"){
    return(dlnorn(value, Hiroshi_shape, Hiroshi_scale))
  } else if(serial_int == "Hiroshi2"){
    return(dweibull(value, shape=Hiroshi2_shape, scale=Hiroshi2_scale))
  } else if(serial_int == "Du"){
    return(dnorm(value, Du_shape, Du_scale))
  }
}

# Calculate daily R0 --- left hand side of the linear regression
number_of_counties = length(unique(county_case$FIPS))

#RDaily <- numeric()
county_case$daily_r <- 0 # create variable to store daily R0
number_of_counties

# Write function to do Wallinga-Teunis
func.WT <- function(daily_inc,serial_int){
  stop <- get(paste0(serial_int, "_stop"))
  RDaily = array(1,length(unique(daily_inc$date)))
  for(u in 1:nrow(daily_inc)){            # for each day in this county
    sumt=0                                # initialize the summation as 0
    for(s in u:(u+stop)){                 # whole summation range: from today to today + stop day
      suma = 0                            # initialize the denominator as 0
      for(a in 0:(stop)){                 # denominator summation range: from 0 to stop day (all possible days)
        suma = if(s>a){daily_inc[s-a,4]*dweibull(a, shape=Li_shape, scale=Li_scale)+suma}else{0+suma}
      } 
      sumt = if(u+stop<=nrow(daily_inc) && suma!=0){(daily_inc[s,4]*dweibull(s-u, shape=Li_shape, scale=Li_scale))/suma + sumt}else if(u+stop<=nrow(daily_inc) && suma!=0){
        sumt}else{sum((daily_inc[s,4]*dweibull(s-u, shape=Li_shape, scale=Li_scale))/suma,sumt,na.rm = TRUE)}
    }
    RDaily[u] = sumt
  }
  return(RDaily)
}  

func.WT(daily_inc,"Li")

county_case$daily_r = 0 # create a variable
for (i in 1:number_of_counties){
  daily_inc = subset(county_case,county_case$FIPS==unique(county_case$FIPS)[i])
  county_case[which(county_case$FIPS==unique(county_case$FIPS)[i]),]$daily_r = func.WT(daily_inc,"Li")
}

write.csv(county_case,paste0("~/Desktop/COVID-19/clean_cases/county_case_with_TWdailyR0_",Sys.Date(),".csv"))

#county_case_truncate05 = county_case[-which(county_case$daily_r<=0 | county_case$daily_r>5),]
# proportion of county left: length(unique(county_case_truncate05$fips))/3007 
#write.csv(county_case_truncate05,"~/Desktop/COVID-19/clean_cases/county_case_with_TWdailyR0_truncate05.csv")

ind = 0
for  (i in 1:number_of_counties){
  ind = append(ind,tail(which(county_case$FIPS==unique(county_case$FIPS)[i]),5))
}

county_case_truncate5 = county_case[-ind,]
write.csv(county_case_truncate05,
          paste0("~/Desktop/COVID-19/clean_cases/county_case_with_TWdailyR0_truncate05",Sys.Date(),".csv"))
  
