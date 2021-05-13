
HEDA_Tidy <- function(kk, season) {
  colnames(kk) <- c("location_id", "datetime","parameter_value")
  kk$datetime <- mdy_hm(kk$datetime)
  kk$mth <- month(kk$datetime)
  kk$yr <- year(kk$datetime)
  kk$dy <- day(kk$datetime)
  kk$nhr <- hour(kk$datetime)
  

  # get rid of negative value
  kk$parameter_value <- ifelse(kk$parameter_value < 0, 0, kk$parameter_value)

  # get annual threshold
  kk$ann_thre <- mean(kk$parameter_value, na.rm = TRUE)
  
    
  # format the datetime
  kk$datetime <- paste(kk$yr, kk$mth, kk$dy, sep = "-")

  kk$datetime <- paste(kk$datetime, " ",kk$nhr, ":00:00", sep = "")
  
  kk <- kk[kk$mth %in% season,]


  # check whether data is empty or not
  if (nrow(kk)>10) {

    # Interpolation data

    ########################################### deal with NA data ######################################################

    ## Get out all the data that have NA
    kk_ <- kk[is.na(kk$parameter_value),]

    ## Count how many NA occurs per year per location_id
    kkt <- kk_ %>% group_by(yr, mth) %>% count()

    # for n > 720, that mth should be removed.
    kkt_720 <- kkt %>% filter(n >= 720)

    # remove these record
    kk <- anti_join(kk,kkt_720, by=c("yr", "mth"))

    #################################################### Interpolation ####################################################################

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
    kk <- kk %>% group_by(location_id,datetime) %>% mutate(parameter_value = mean(parameter_value)) %>% distinct(parameter_value,ann_thre) %>% ungroup()

  }
  return(kk)
}
