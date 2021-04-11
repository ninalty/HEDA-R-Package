
#--------------------Step 6 count the reversal
library(dplyr)
library(lubridate)
library(zoo)

# set working directory
HPK_Path <- "D:/Ninalty/UCD_Hydropeaking/HPK_FlowData/HPK_CA/HPK_sm"

site_list <- list.files(path = HPK_Path, "_ct.csv")#remove sites not needed

for (j in 1:length(site_list)) {
  
  kk = read.csv(paste(HPK_Path, site_list[j], sep = ""))# check, might get error
  
  kk <- kk %>% mutate(nhour = hour(datetime)) %>% mutate(datetime = date(datetime))
  
  #----------------------- Identify the up and dw process of hydropeaking
  kk <- up_dw_ID(kk) 
  
  if (length(na.omit(kk$up))>2) {
    
    #----------------------- count daily pk ----------------------
    HPK_SM_updw_Daily <- up_dw_count(kk)
    HPK_SM_updw_Daily$location_id <- kk$location_id[1]
    
    HPK_SM_updw_Daily$pkratio <- PK_Ratio(kk,HPK_SM_updw_Daily) # will get an error, it is fine
    
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


print("Metrcis extraction Done!")
