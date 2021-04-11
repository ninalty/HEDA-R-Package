#
library(dplyr)
library(lubridate)

#plz remove those with size < 1KB
file_path <-"D:/Ninalty/UCD_Hydropeaking/HPK_FlowData/HPK_RDS/HPK_AZ/HPK_sm_AZ/"

setwd(file_path)

file_list <- list.files(path = file_path)

flow_data <- bind_rows(lapply(file_list, read.csv)) 

flow_data$yr <- year(flow_data$datetime)

# ge the year of each gauges
tt <- flow_data %>% group_by(location_id) %>% mutate(yr_strt = min(yr)) %>% mutate(yr_end = max(yr)) %>% ungroup()

# get the start and end year
tt2 <- tt %>% group_by(location_id) %>% distinct(yr_strt, yr_end)

tt <- tt %>% group_by(location_id) %>% distinct(yr) %>% count()

tt3 <- left_join(tt, tt2, "location_id")

write.csv(tt3, file = "D:/Ninalty/UCD_Hydropeaking/HPK_Stats/AZ_site_info.csv")
