library(dplyr)
library(lubridate)
library(zoo)

#Identify the up and dw
up_dw_ID <- function(df){

  df$up <- rep(NA, nrow(df))
  df$dw <- rep(NA, nrow(df))
  index_lt <- which(df$dgtag %in% c(1,2,3,4))
  n <- length(index_lt)-1

  tt <- 1
  if(n>3){
    for (i in 2:n) {
      t <- index_lt[i-1]
      l <- index_lt[i]
      m <- index_lt[i+1]

      if (df$dgtag[l]==4 & df$dgtag[m]==2) {
        tt=tt+1
        df$up[l]<-"up0"
        df$up[m]<-"up"}

      if (df$dgtag[l]==4 & df$dgtag[m]==3) {
        tt=tt+1
        df$up[l]<-"up0"
        df$up[m]<-"up"}

      if (df$dgtag[l]==1 & df$dgtag[m]==3) {
        tt=tt+1
        df$up[l]<-"up0"
        df$up[m]<-"up"}

      if (df$dgtag[l]==1 & df$dgtag[m]==2) {
        tt=tt+1
        df$up[l]<-"up0"
        df$up[m]<-"up"}

      if (df$dgtag[l]==3 & df$dgtag[m]==2) {
        tt=tt+1
        df$dw[l]<-"dw0"
        df$dw[m]<-"dw"}

      if (df$dgtag[l]==3 & df$dgtag[m]==1) {
        tt=tt+1
        df$dw[l]<-"dw0"
        df$dw[m]<-"dw"}

      if (df$dgtag[t]==2 & df$dgtag[l]==3 & df$dgtag[m]==2) {
        tt=tt+1
        df$dw[l]<-"dw0"
        df$dw[m]<-"dw"}

      if (df$dgtag[t]==2 & df$dgtag[l]==3 & df$dgtag[m]==1) {
        tt=tt+1
        df$dw[l]<-"dw0"
        df$dw[m]<-"dw"}}}

  return(df)}

PK_Pb <- function(df){
  df = table(df)
  df = data.frame(df/sum(df)*100)
  names(df) <- c("Var","Freq")
  df$Var <- as.numeric(levels(df$Var))
  df$Freq <- as.numeric(df$Freq)
  df <- with(df, sum(Var*Freq/100))
  return(df)}

# count pk_no
up_dw_count <- function(df){

  up_no <- df %>% group_by(datetime) %>% filter(up=="up0") %>% count()
  dw_no <- df %>% group_by(datetime) %>% filter(dw=="dw0") %>% count()

  daily_pk_no <- full_join(up_no,dw_no, by="datetime")

  # fix the na value of n
  daily_pk_no$n.x <- ifelse(is.na(daily_pk_no$n.x), 0, daily_pk_no$n.x)
  daily_pk_no$n.y <- ifelse(is.na(daily_pk_no$n.y), 0, daily_pk_no$n.y)
  daily_pk_no$yr <- year(daily_pk_no$datetime)

  #add up and dw together by day
  daily_pk_no$pk_no <- (daily_pk_no$n.x + daily_pk_no$n.y) #not devided by 2

  return(daily_pk_no)}


#calculate the pk ratio
PK_Ratio <- function(df1, df2){

  row1 <- nrow(df1)
  row2 <- nrow(df2)
  row_no <- ifelse(row1!=0, row2*24/row1*100, 0)

  return(row_no)
}


# ramping rate
HPK_ramprt_duration <- function(df){

  df <- df %>% mutate(datetime2 = paste(datetime, " ", nhour, ":00:00", sep = ""))

  df$D_up <- rep(NA, nrow(df)) # the duration of peaking events last at high flow, or the peaking retention
  df$D_dw <- rep(NA, nrow(df)) # the rest time between two peaking events.
  df$D_rampdw <- rep(NA, nrow(df)) # the time that used to complete one down ramping process
  df$D_rampup <- rep(NA, nrow(df)) # the time used to complete one up ramping process
  df$Ramp_dw <- rep(NA, nrow(df)) # down ramping rate dQ/dt
  df$Ramp_up <- rep(NA, nrow(df)) # up ramping rate
  df$Strange_dw <- rep(NA, nrow(df)) # unit down ramping rate dQ/Aave
  df$Strange_up <- rep(NA, nrow(df)) # unit up ramping rate dQ/Aave
  df$RB_Index_dw <- rep(NA, nrow(df))
  df$RB_Index_up <- rep(NA, nrow(df))

  index_lt <- which(df$dgtag %in% c(1,2,3,4))
  n <- length(index_lt)-1

  if(n>4){
    for (i in 1:n) {
      l <- index_lt[i]
      m <- index_lt[i+1]
      if (as.numeric(difftime(df$datetime2[m],df$datetime2[l], units = "days")) < 8 ){

        #pk duration
        if (df$dgtag[l]==2 & df$dgtag[m]==3) {
          df$D_up[l] = as.numeric(difftime(df$datetime2[m], df$datetime2[l]))}

        if (df$dgtag[l]==2 & df$dgtag[m]==4) {
          df$D_dw[l] = as.numeric(difftime(df$datetime2[m], df$datetime2[l]))}

        if (df$dgtag[l]!=2 & df$dgtag[m]==3) {
          df$D_dw[m] = 0}

        if (df$dgtag[l]==1) {
          df$D_dw[l] = 0}

        #ramping duration
        if (df$dgtag[l]==4 & df$dgtag[m]==2) {
          df$D_rampup[l] = as.numeric(difftime(df$datetime2[m], df$datetime2[l]))}

        if (df$dgtag[l]==1 & df$dgtag[m]==3) {
          df$D_rampup[l] = as.numeric(difftime(df$datetime2[m], df$datetime2[l]))}

        if (df$dgtag[l]==4 & df$dgtag[m]==3) {
          df$D_rampup[l] = as.numeric(difftime(df$datetime2[m], df$datetime2[l]))}

        if (df$dgtag[l]==3 & df$dgtag[m]==1) {
          df$D_rampdw[l] = as.numeric(difftime(df$datetime2[m], df$datetime2[l]))}

        if (df$dgtag[l]==3 & df$dgtag[m]==2) {
          df$D_rampdw[l] = as.numeric(difftime(df$datetime2[m], df$datetime2[l]))}

        ## dq/dt
        # calculate the dw ramping rate
        if (df$dgtag[l]==3 & df$dgtag[m]==1) {
          df$Ramp_dw[l] = abs(df$parameter_value[l]-df$parameter_value[m])/as.numeric(difftime(df$datetime2[m], df$datetime2[l]))
          df$Strange_dw[l] = abs(df$parameter_value[l]-df$parameter_value[m])/(df$ann_thre[1]+1) #to avoid zero
          df$RB_Index_dw[l] = abs(df$parameter_value[l]-df$parameter_value[m])/((df$ann_thre[1]+1)*as.numeric(difftime(df$datetime2[m], df$datetime2[l])))
        }

        # calculate the dw ramping rate
        if (df$dgtag[l]==3 & df$dgtag[m]==2) {
          df$Ramp_dw[l] = abs(df$parameter_value[l]-df$parameter_value[m])/as.numeric(difftime(df$datetime2[m], df$datetime2[l]))
          df$Strange_dw[l] = abs(df$parameter_value[l]-df$parameter_value[m])/(df$ann_thre[1]+1)
          df$RB_Index_dw[l] = abs(df$parameter_value[l]-df$parameter_value[m])/((df$ann_thre[1]+1)*as.numeric(difftime(df$datetime2[m], df$datetime2[l])))
        }

        # calculate the up ramping rate
        if (df$dgtag[l]==4 & df$dgtag[m]==2) {
          df$Ramp_up[l] = abs(df$parameter_value[l]-df$parameter_value[m])/as.numeric(difftime(df$datetime2[m], df$datetime2[l]))
          df$Strange_up[l] = abs(df$parameter_value[l]-df$parameter_value[m])/(df$ann_thre[1]+1) #to avoid zero
          df$RB_Index_up[l] = abs(df$parameter_value[l]-df$parameter_value[m])/((df$ann_thre[1]+1)*as.numeric(difftime(df$datetime2[m], df$datetime2[l])))
        }

        # calculate the up ramping rate
        if (df$dgtag[l]==1 & df$dgtag[m]==3) {
          df$Ramp_up[l] = abs(df$parameter_value[l]-df$parameter_value[m])/as.numeric(difftime(df$datetime2[m], df$datetime2[l]))
          df$Strange_up[l] = abs(df$parameter_value[l]-df$parameter_value[m])/(df$ann_thre[1]+1) #to avoid zero
          df$RB_Index_up[l] = abs(df$parameter_value[l]-df$parameter_value[m])/((df$ann_thre[1]+1)*as.numeric(difftime(df$datetime2[m], df$datetime2[l])))
        }

        # calculate the up ramping rate
        if (df$dgtag[l]==4 & df$dgtag[m]==3) {
          df$Ramp_up[l] = abs(df$parameter_value[l]-df$parameter_value[m])/as.numeric(difftime(df$datetime2[m], df$datetime2[l]))
          df$Strange_up[l] = abs(df$parameter_value[l]-df$parameter_value[m])/(df$ann_thre[1]+1) #to avoid zero
          df$RB_Index_up[l] = abs(df$parameter_value[l]-df$parameter_value[m])/((df$ann_thre[1]+1)*as.numeric(difftime(df$datetime2[m], df$datetime2[l])))
        }

        df$D_up <- ifelse(df$D_up > 12, NA, df$D_up)
        df$D_dw <- ifelse(df$D_dw > 24, NA, df$D_dw)
        df$D_rampup <- ifelse(df$D_rampup > 12, NA, df$D_rampup)
        df$D_rampdw <- ifelse(df$D_rampdw > 12, NA, df$D_rampdw)
        df$Ramp_dw <- ifelse(df$Ramp_dw < 5, NA, df$Ramp_dw)

      }}}
  return(df)
}

# ---------------------The exported function-----------------
#' @export
HPK_metrics <- function(filePath) {
  kk = read.table(filePath)
  kk <- kk %>% mutate(nhour = lubridate::hour(datetime)) %>% mutate(datetime = lubridate::date(datetime))

  #----------------------- Identify the up and dw process of hydropeaking
  kk <- up_dw_ID(kk)

  if (length(na.omit(kk$up))>2) {

    #----------------------- count daily pk ----------------------
    HPK_SM_updw_Daily <- up_dw_count(kk)
    HPK_SM_updw_Daily$location_id <- kk$location_id[1]

    HPK_SM_updw_Daily$pkratio <- PK_Ratio(kk,HPK_SM_updw_Daily)

    #----------------------- Magnitude --------------------
    # to get the 95th of the peaking Q
    Q_pk_max <-  rbind(kk[which(kk$up=="up" & kk$dgtag==2),], kk[which(kk$up=="up" & kk$dgtag==3),])

    # to get the 5th of the peaking Q
    Q_pk_min <- rbind(kk[which(kk$dw=="dw" & kk$dgtag==2),], kk[which(kk$up=="dw" & kk$dgtag==1),])

    #-----------------------  Duration & Ramp rate  ---------------
    HPK_SM_Ramrt_duration <-  HPK_ramprt_duration(kk)

    #----------------------- Round 2  ---------------------
    # magnitude was divided by the ann_threshold
    Qpeak <- quantile(Q_pk_max$parameter_value, probs = 1)/Q_pk_max$ann_thre[1]

    Qoff_peak <- quantile(Q_pk_min$parameter_value, probs = 0)/Q_pk_min$ann_thre[1]

    # ratio of hpk days
    pk_ratio <- HPK_SM_updw_Daily$pkratio[1]

    Pk_No <- mean(HPK_SM_updw_Daily$pk_no, na.rm = TRUE)

    Pk_retention <- median(HPK_SM_Ramrt_duration$D_up, na.rm = TRUE)
    Offpk_Retention <- median(HPK_SM_Ramrt_duration$D_dw, na.rm = TRUE)

    D_rampdw <- median(HPK_SM_Ramrt_duration$D_rampdw, na.rm = TRUE)
    D_rampup <- median(HPK_SM_Ramrt_duration$D_rampup, na.rm = TRUE)

    RB_Index_dw <- median(HPK_SM_Ramrt_duration$RB_Index_dw, na.rm = TRUE)
    RB_Index_up <- median(HPK_SM_Ramrt_duration$RB_Index_up, na.rm = TRUE)

    Ramp_dw <- median(HPK_SM_Ramrt_duration$Ramp_dw, na.rm = TRUE)
    Ramp_up <- median(HPK_SM_Ramrt_duration$Ramp_up, na.rm = TRUE)

    Strange_dw <- median(HPK_SM_Ramrt_duration$Strange_dw, na.rm = TRUE)
    Strange_up <- median(HPK_SM_Ramrt_duration$Strange_up, na.rm = TRUE)

    Tmax <- PK_Pb(Q_pk_max$nhour)

    #-----------------------  Make the metrics  ------------------------
    HPK_SM_metric <- data.frame(kk$location_id[1], Pk_No, Qpeak, Qoff_peak, Pk_retention, Offpk_Retention, Ramp_dw, Ramp_up, D_rampdw, D_rampup, Strange_dw, Strange_up, pk_ratio, Tmax, RB_Index_dw,RB_Index_up)

    row.names(HPK_SM_metric) <- HPK_SM_metric[,1]
    HPK_SM_metric <- HPK_SM_metric[,-1]

    write.csv(HPK_SM_metric,paste(HPK_Path, kk$location_id[1], "_metric.csv", sep = ""))
  }
}
