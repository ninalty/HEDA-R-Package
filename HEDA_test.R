library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)

# import the flow data
df1 <- read.table("D:/Ninalty/UCD_Hydropeaking/HPK_FlowData/HPK_WC/CA/11278400.csv",sep = ",",header = TRUE, row.names = NULL)

# set the output file path for HEDA_Tidy()
filepath = "D:/Ninalty/UCD_Hydropeaking/HPK_FlowData/HEDA_Sampledata/"


#----------------- clean and interpolate
hpk_flow_sm_cln = HEDA_Tidy(df1, season = c(6,7,8,9))

#save that data in csv file, recommended
# write.table(hpk_flow_sm_cln, paste(filepath, hpk_flow_sm_cln$location_id[1],"_flow_sm.csv", sep = ""), sep=",", row.names = FALSE)


#######################################      Hyddropeaking events detection        ########################

# 60 is for metric units, corrresponding to a rate of change of 1.7m3/s.
hpk_flow_cg <- ReversalCount(hpk_flow_sm_cln, alpha1 = 0.03, theta = 85)



#######################################       Clean noise       ########################
hpk_flow_cg <- clean_position(hpk_flow_cg, alpha2 = 0.3)

# clean_Spt and clean_conectD are suggested to repeat twice. The performance can be checked with HPK_plot()
hpk_flow_cg <- clean_Spt(hpk_flow_cg, alpha3 = 0.7, alpha4 = 0.5)

hpk_flow_cg <- clean_Spt(hpk_flow_cg)

hpk_flow_cg <- clean_conectD(hpk_flow_cg, alpha3 = 0.7, alpha4 = 0.5)

hpk_flow_cg <- clean_Spt(hpk_flow_cg)


#######################################           Visualization        #############################
# subset the data
tt = hpk_flow_cg[13100:13600,]

# plot the hydrograph of the subset data
HPK_plot(tt)


####################################            Extract metrics            ###############
hpk_flow_metric <- HPK_metrics(hpk_flow_cg)

write.table(hpk_flow_metric,paste(filepath, hpk_flow_metric$location_id[1], "_metric_sm.csv", sep = ""), sep=",", row.names = FALSE)
