# Initial setup 
# 1.  Load Historical_Simulation_Data.csv dataset into R
# 2.  Download and install the scatterplot3d package from 
#     http://cran.r-project.org/web/packages/scatterplot3d/index.html

# Include the scatterplot3d library
library(scatterplot3d)

# Peak Day Data: Demand, Maximum Temperature, and Minimum Temperature
#   49 Year Range: 1966 - 2014
max_temp = Historical_Simulation_Data$Max_Temp
min_temp = Historical_Simulation_Data$Min_Temp
peak_demand = Historical_Simulation_Data$Peak_Demand

# Create a function that outputs the peak
#   demand given input max and min temp values.
get_nel = function(min,max){
  nel = Historical_Simulation_Data$Peak_Demand[
    Historical_Simulation_Data$Max_Temp==max & Historical_Simulation_Data$Min_Temp == min]
    return(nel)
}

# 3D Scatterplot with Coloring and Linear Regression Plane 
s3d <-scatterplot3d(max_temp,min_temp, peak_demand, pch=16, highlight.3d=TRUE)               
fit <- lm(peak_demand ~ max_temp+min_temp)
s3d$plane3d(fit)

# 3D Scatterplot with Linear Regression Plane, Coloring, and Vertical Lines
# s3d <-scatterplot3d(max_temp,min_temp, peak_demand, pch=16, highlight.3d=TRUE, type="h")
# fit <- lm(peak_demand ~ max_temp+min_temp)
# s3d$plane3d(fit)

# Overlay normal curve to histogram of maximum temperature
h = hist(max_temp, breaks=21, col="red", main="Maximum Temperature") 
xfit<-seq(min(max_temp),max(max_temp),length=40) 
yfit<-dnorm(xfit,mean=mean(max_temp),sd=sd(max_temp)) 
yfit <- yfit*diff(h$mids[1:2])*length(max_temp) 
lines(xfit, yfit, col="black", lwd=2)
box()

# Overlay normal curve to histogram of minimum temperature
h = hist(min_temp, breaks=15,col="blue", main="Minimum Temperature") 
xfit<-seq(min(min_temp),max(min_temp),length=40) 
yfit<-dnorm(xfit,mean=mean(min_temp),sd=sd(min_temp)) 
yfit <- yfit*diff(h$mids[1:2])*length(min_temp) 
lines(xfit, yfit, col="black", lwd=2)
box()

# Overlay normal curve to histogram of simulated peak demands 
h = hist(peak_demand, breaks=21,col="green", main="Simulated Peak Demand") 
xfit<-seq(min(peak_demand),max(peak_demand),length=40) 
yfit<-dnorm(xfit,mean=mean(peak_demand),sd=sd(peak_demand)) 
yfit <- yfit*diff(h$mids[1:2])*length(peak_demand) 
lines(xfit, yfit, col="black", lwd=2)
box()