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