library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)

# import the flow data
df1 <- read.table(".../.../...ID.csv",sep = ",",col.names=c("lcoation_id","dateTime","parameter_value"), header = TRUE, row.names = NULL)

# set the output file path for HEDA_Tidy()
filepath = "D:/Ninalty/UCD_Hydropeaking/HPK_FlowData/HEDA_Sampledata/"

HEDA_Tidy(df1, dirPathForSM = filepath, dirPathForWT = filepath)

#######################################      Hyddropeaking events detection        ########################
# to run the following functions, data needs to be imported again. Dry season data would be recommended for better performance.
df1 <- read.table("D:/Ninalty/UCD_Hydropeaking/HPK_FlowData/HEDA_Sampledata/11278400_t.csv",sep = ",", header = TRUE)

# just to in case, format the time column again.
df1$datetime <- ymd_hms(df1$datetime)

# 60 is for metric units, corrresponding to a rate of change of 1.7m3/s.
df1 <- ReversalCount(df1, aerfa1 = 0.03, theta = 60)

#######################################       Clean noise       ########################
df2 <- clean_position(df1, aerfa2)

# clean_Spt and clean_conectD are suggested to repeat twice. The performance can be checked with HPK_plot()
df2 <- clean_Spt(df1)

df2 <- clean_Spt(df1)

df2 <- clean_conectD(df1)

df2 <- clean_Spt(df1)

#######################################           Visualization        #############################
# subset the data
tt = df1[13000:13500,]

# plot the hydrograph of the subset data
HPK_plot(tt)

