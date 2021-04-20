library(dplyr)
library(lubridate)
library(zoo)


HEDA_Tidy <- function(filePath, dirPathForSM, dirPathForWT) {
  kk <- read.csv(filePath)
  colnames(kk) <- c("location_id", "datetime","parameter_value")
  #kk$datetime <- as.POSIXct(strptime(kk$datetime, "%m/%d/%Y %H:%M"))
  #kk$datetime <- anytime(kk$datetime)
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

  #--------------------------------------------- dry season ---------------------------#
  # check whether data is empty or not
  if (nrow(hpk_flow_sm)>10) {

    # format the datetime
    #print(hpk_flow_sm$datetime)
    hpk_flow_sm$datetime <- lubridate::date(hpk_flow_sm$datetime)

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

    ##interpolate data
    for (i in 2:nrow(kk)) {
      if (is.na(kk$parameter_value[i])){
        if(!is.na(kk$parameter_value[i-1])&!is.na(kk$parameter_value[i+1])){
          kk$parameter_value[i] = (kk$parameter_value[i-1]+kk$parameter_value[i+1])/2}
      }
      if (is.na(kk$parameter_value[i])){
        if(!is.na(kk$parameter_value[i-1])&!is.na(kk$parameter_value[i+2])){
          kk$parameter_value[i] = (kk$parameter_value[i-1]+kk$parameter_value[i+2])/2}
      }
      if (is.na(kk$parameter_value[i])){
        if(!is.na(kk$parameter_value[i-1])){
          kk$parameter_value[i] = kk$parameter_value[i-1]}
      }
    }

    # keep one flow for one hour
    kk <- kk %>% group_by(location_id,datetime) %>% distinct(parameter_value,ann_thre) %>% ungroup()
    #kk$datetime <- format(kk$datetime, "%Y-%m-%d %H:%M")
    #save that data in csv file
    write.table(kk, paste(dirPathForSM, kk$location_id[1],"_flow_sm.csv", sep = ""))
    #write.csv(kk, paste(dirPathForSM, kk$location_id[1],"_flow_sm.csv", sep = ""))

    remove(kk,kk_,kkt,kkt_720,i)
  }


# ---------------------- wet season -------------------------------------
  if (nrow(hpk_flow_wt)>10) {

    # format the datetime
    hpk_flow_wt$datetime <- lubridate::date(hpk_flow_wt$datetime)

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

    ##interpolate data
    for (i in 2:nrow(kk)) {
      if (is.na(kk$parameter_value[i])){
        if(!is.na(kk$parameter_value[i-1])&!is.na(kk$parameter_value[i+1])){
          kk$parameter_value[i] = (kk$parameter_value[i-1]+kk$parameter_value[i+1])/2}
      }
      if (is.na(kk$parameter_value[i])){
        if(!is.na(kk$parameter_value[i-1])&!is.na(kk$parameter_value[i+2])){
          kk$parameter_value[i] = (kk$parameter_value[i-1]+kk$parameter_value[i+2])/2}
      }
      if (is.na(kk$parameter_value[i])){
        if(!is.na(kk$parameter_value[i-1])){
          kk$parameter_value[i] = kk$parameter_value[i-1]}
      }
    }


    # keep one flow for one hour
    kk <- kk %>% group_by(location_id,datetime) %>% distinct(parameter_value,ann_thre) %>% ungroup()

    #save that data in csv file
    write.table(kk, paste(dirPathForWT, kk$location_id[1],"_flow_wt.csv", sep = ""))
    #kk$datetime <- format(kk$datetime, "%Y-%m-%d %H:%M")
    #write.csv(kk, paste(dirPathForWT, kk$location_id[1],"_flow_wt.csv", sep = ""))

    remove(kk,kk_,kkt,kkt_720,i)
  }
}
