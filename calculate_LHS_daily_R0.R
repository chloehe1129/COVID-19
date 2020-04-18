setwd("~/Desktop/COVID-19/us data/COVID_data")
county_case <- read.csv("us-counties.csv") # probably need to pull daily updated dataset from JHU github
county_case <- county_case[order(county_case$fips),] # order based on fips number 

# Set max generation interval as first day that captures >99% of the density
library(R0)
a <- est.GT(serial.interval = daily_inc$cases) # get an estimation on the serial interval distribution
shape <- a$mean
scale <- a$sd
stop <- ceiling(qgamma(0.99, shape=shape, scale=scale)) # approximate it with GAMMA distribution

stop = 10 # LET for now

## function to pull value of serial interval distribution at any time
# dweibull(time, shape=shape, scale=scale)


# Calculate daily R0 --- left hand side of the linear regression
number_of_counties = length(unique(county_case$fips))

RDaily <- numeric()
county_case$daily_r <- 0 # create variable to store daily R0

for (i in 1:(number_of_counties)){
  daily_inc = subset(county_case,county_case$fips==unique(county_case$fips)[i])
  for(u in 1:(length(unique(daily_inc$date)))){ #Estimate for each day
      sumt <- 0 # numerator
      for(t in u:(u+stop)){ #Look ahead starting at day u through (u+max SI)
        suma <- 0 # denominator
        for(a in 0:(stop)){ #Calc denominator, from day t back through (t-max SI)
          suma = daily_inc[t-a,5]*dweibull(a, shape=shape, scale=scale)+suma
        } 
        sumt = (daily_inc[t,5]*dweibull(t-u, shape=shape, scale=scale))/suma + sumt
      }
      RDaily[u] = sumt
      county_case[which(county_case$fips==unique(county_case$fips)[i]),]$daily_r = RDaily
  }
}


