Peak_Demand_Model_Data <- read.csv("~/Electricity_Peak_Demand_Analysis/Peak_Demand_Model_Data.csv")
Summer_Weekday<-subset(Peak_Demand_Model_Data,Bad==0,select=Max_NEL_wo_DC_Loss:Bad)
fit<-lm(Max_NEL_wo_DC_Loss~Weather_75+Weather_80+Weather_85+Weather_90+Weather_95+Weather_100+Weather_105+Jun+Jul+Sep+Year2008+Year2009+Year2010+Year2011+Year2012+Year2013+Min_Temp_60+Min_Temp_65+Min_Temp_70,data=Summer_Weekday)
summary(fit)
