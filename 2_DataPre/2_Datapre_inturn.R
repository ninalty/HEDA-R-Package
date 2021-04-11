library(dplyr)
library(lubridate)
library(zoo)

#plz remove those with size < 1KB

file_path <-"D:/Ninalty/UCD_Hydropeaking/HPK_FlowData/HPK_WC/Colorado/"
file_path2 <-"D:/Ninalty/UCD_Hydropeaking/HPK_FlowData/HPK_RDS/HPK_Cloda/HPK_sm_Cloda/"

Loc_id <- list.files(file_path,".csv")

for (j in 1: length(Loc_id)) {
  
  kk = read.csv(paste(file_path, Loc_id[j], sep = ""))
  
  kk = kk[,c(3,4,5)]
  
  colnames(kk) <- c("location_id", "datetime","parameter_value")
  
  # Split data into seasons
  kk$mth <- month(kk$datetime)
  kk$yr <- year(kk$datetime)
  kk$nhr <- hour(kk$datetime)
  
  # get rid of negative value
  kk$parameter_value <- ifelse(kk$parameter_value<0, 0, kk$parameter_value)
  
  # get annual threshold
  kk$ann_thre <- mean(kk$parameter_value, na.rm = TRUE)

  #summer is fined as month June-September
  hpk_flow_sm <- kk[kk$mth %in% c(6,7,8,9),]
  
  # check whether still have data
  if (nrow(hpk_flow_sm)>10) {
    
    hpk_flow_sm$datetime <- date(hpk_flow_sm$datetime)
  
    hpk_flow_sm$datetime <- paste(hpk_flow_sm$datetime, " ",hpk_flow_sm$nhr, ":00:00", sep = "")
    
    # convert all flow to hour flow
    hpk_flow_sm <- hpk_flow_sm %>% group_by(location_id,datetime) %>% mutate(parameter_value = mean(parameter_value)) %>% ungroup()

    
    # Interpolation data
    
    ########################################### data need to remove ######################################################
    
    ## pk_flowdata_32_summer.rds ## is the dataset without removing NA
    kk = hpk_flow_sm
  
    ## Get out all the data that have NA
    kk_ <- kk[is.na(kk$parameter_value),]
    
    ## Count how many NA occurs per year per location_id 
    kkt <- kk_ %>% group_by(yr, mth) %>% count()
    

    # for n > 720, that mth should be removed.
    kkt_720 <- kkt %>% filter(n >= 720)
    
    # remove these record
    kk <- anti_join(kk,kkt_720, by=c("yr", "mth"))
    
    #################################################### Interpolation ####################################################################
    # this is too slow, needs to improve if to process US data
    
    ##interpolate data--1
    for (i in 2:nrow(kk)) {
      if (is.na(kk$parameter_value[i])){
        if(!is.na(kk$parameter_value[i-1])&!is.na(kk$parameter_value[i+1])){
          kk$parameter_value[i] = (kk$parameter_value[i-1]+kk$parameter_value[i+1])/2}
      } 
      i=i+1
    }
    
    ##interpolate data --2
    for (i in 2:nrow(kk)) {
      if (is.na(kk$parameter_value[i])){
        if(!is.na(kk$parameter_value[i-1])&!is.na(kk$parameter_value[i+2])){
          kk$parameter_value[i] = (kk$parameter_value[i-1]+kk$parameter_value[i+2])/2}
      } 
      i=i+1
    }
    
    ##interpolate data --3
    for (i in 2:nrow(kk)) {
      if (is.na(kk$parameter_value[i])){
        if(!is.na(kk$parameter_value[i-1])){
          kk$parameter_value[i] = kk$parameter_value[i-1]}
      } 
      i=i+1
    }
    
    # keep one flow for one hour
    kk <- kk %>% group_by(location_id,datetime) %>% distinct(parameter_value,ann_thre) %>% ungroup()
    
    #save that data
    write.csv(kk, paste(file_path2, kk$location_id[1],"_flow_sm.csv", sep = ""))

    remove(kk,kk_,kkt,kkt_720,i)    
    }
}





