library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)

# import the flow data
df <- HPK_SampleData


# clean and interpolate
## format the time column to time format
df$dateTime <- parse_date_time(df$dateTime, "mdy HM")

# clean subset data by season
hpk_flow_sm_cln = HEDA_Tidy(df, season = c(6,7,8,9))


# Hyddropeaking events detection
## identify hydropeaking events
### 60 is for metric units, corrresponding to a rate of change of 1.7m3/s.
hpk_flow_cg <- ReversalCount(hpk_flow_cln, alpha1 = 0.03, theta = 85)


# Clean noise
hpk_flow_cg <- Clean_position(hpk_flow_cg, alpha2 = 0.3)

# Clean_Spt and Clean_conectD are suggested to repeat twice. The performance can be checked with HPK_plot()
hpk_flow_cg <- Clean_Spt(hpk_flow_cg, alpha3 = 0.7, alpha4 = 0.5)

hpk_flow_cg <- Clean_Spt(hpk_flow_cg)

hpk_flow_cg <- Clean_conectD(hpk_flow_cg, alpha3 = 0.7, alpha4 = 0.5)

hpk_flow_cg <- Clean_Spt(hpk_flow_cg)


# Visualization
## subset the data
tt = hpk_flow_cg[13100:13600,]

# plot the hydrograph of the subset data
HPK_plot(tt)


# Extract metrics
HpkFrqMgt <- HPK_frq_mgt(hpk_flow_cg)
HpkRtDur <- HPK_rt_dur(hpk_flow_cg)


# to extract time series metric
pk_no <- HpkFrqMgt[na.omit(HpkFrqMgt$pk_no),c("location_id", "datetime","pk_no")]
