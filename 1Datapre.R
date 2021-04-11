library(dplyr)
library(lubridate)
library(zoo)

#plz remove those with size < 1KB

file_path <-"D:/Ninalty/UCD_Hydropeaking/HPK_FlowData/HPK_CA/"
file_path2 <-"D:/Ninalty/UCD_Hydropeaking/HPK_FlowData/HPK_CA/HPK_sm/"
file_path3 <-"D:/Ninalty/UCD_Hydropeaking/HPK_FlowData/HPK_CA/HPK_wt/"

Loc_id <- list.files(file_path,".csv")

# input data should have 3 columns only, hourly data.

for (j in 1: length(Loc_id)) {
  
  kk = read.csv(paste(file_path, Loc_id[j], sep = ""))
  
  #kk = kk[,c(3,4,5)]
  
  # change column names
  colnames(kk) <- c("location_id", "datetime","parameter_value")
  
  # Split data by seasons
  kk$mth <- month(kk$datetime)
  kk$yr <- year(kk$datetime)
  kk$nhr <- hour(kk$datetime)
  
  # get rid of negative value
  kk$parameter_value <- ifelse(kk$parameter_value<0, 0, kk$parameter_value)
  
  # get annual threshold
  kk$ann_thre <- mean(kk$parameter_value, na.rm = TRUE)

  # summer is fined as month June-September
  hpk_flow_sm <- kk[kk$mth %in% c(6,7,8,9),]
  
  # leftover is wet season.
  hpk_flow_wt <- kk[!kk$mth %in% c(6,7,8,9),]
  
  ## originally, i manually run this for dry and wet season. Now I want to have interpolated outputs for two seasons. Maybe a function is needed?
  
 #--------------------------------------------- dry season ---------------------------#
  # check whether data is empty or not
  if (nrow(hpk_flow_sm)>10) {
    
	# format the datetime 
    hpk_flow_sm$datetime <- date(hpk_flow_sm$datetime)
  
    hpk_flow_sm$datetime <- paste(hpk_flow_sm$datetime, " ",hpk_flow_sm$nhr, ":00:00", sep = "")

    
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
    # this is too slow, needs to improve, any ideas?
    
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
    
    #save that data in csv file
    write.csv(kk, paste(file_path2, kk$location_id[1],"_flow_sm.csv", sep = ""))

    remove(kk,kk_,kkt,kkt_720,i)    
    }
}

# ---------------------- wet season -------------------------------------
  if (nrow(hpk_flow_wt)>10) {
    
	# format the datetime 
    hpk_flow_wt$datetime <- date(hpk_flow_wt$datetime)
  
    hpk_flow_wt$datetime <- paste(hpk_flow_wt$datetime, " ",hpk_flow_wt$nhr, ":00:00", sep = "")
    
    
    # Interpolation data
    
    ########################################### data need to remove ######################################################
    
    ## pk_flowdata_32_summer.rds ## is the dataset without removing NA
    kk = hpk_flow_wt
  
    ## Get out all the data that have NA
    kk_ <- kk[is.na(kk$parameter_value),]
    
    ## Count how many NA occurs per year per location_id 
    kkt <- kk_ %>% group_by(yr, mth) %>% count()
    

    # for n > 720, that mth should be removed.
    kkt_720 <- kkt %>% filter(n >= 720)
    
    # remove these record
    kk <- anti_join(kk,kkt_720, by=c("yr", "mth"))
    
    #################################################### Interpolation ####################################################################
    # this is too slow, needs to improve, any ideas?
    
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
    
    #save that data in csv file
    write.csv(kk, paste(file_path3, kk$location_id[1],"_flow_wt.csv", sep = ""))

    remove(kk,kk_,kkt,kkt_720,i)    
    }
}



