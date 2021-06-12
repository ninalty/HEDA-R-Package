# calculate duration and rate of change
HPK_ramprt_duration <- function(df){

  df$pk_rtn <- rep(NA, nrow(df))
  df$offpk_rtn <- rep(NA, nrow(df))
  df$D_rampdw <- rep(NA, nrow(df))
  df$D_rampup <- rep(NA, nrow(df))
  df$Ramp_dw <- rep(NA, nrow(df))
  df$Ramp_up <- rep(NA, nrow(df))
  df$Strange_dw <- rep(NA, nrow(df))
  df$Strange_up <- rep(NA, nrow(df))
  df$RB_Index_dw <- rep(NA, nrow(df))
  df$RB_Index_up <- rep(NA, nrow(df))

  index_lt <- which(df$dgtag %in% c(1,2,3,4))
  n <- length(index_lt)-1

  if(n>4){
    for (i in 1:n) {
      l <- index_lt[i]
      m <- index_lt[i+1]
      if (as.numeric(difftime(df$datetime[m],df$datetime[l], units = "days")) < 8 ){# Continuouse change points should occur within a week.

        #pk and offpk duration
        if (df$dgtag[l]==2 & df$dgtag[m]==3) {
          df$pk_rtn[l] = as.numeric(difftime(df$datetime[m], df$datetime[l]))}

        if (df$dgtag[l]==2 & df$dgtag[m]==4) {
          df$offpk_rtn[l] = as.numeric(difftime(df$datetime[m], df$datetime[l]))}

        if (df$dgtag[l]!=2 & df$dgtag[m]==3) {
          df$offpk_rtn[m] = 0}

        if (df$dgtag[l]==1) {
          df$offpk_rtn[l] = 0}

        #metrics relate to rise/fall process
        if (df$dgtag[l]==4 & df$dgtag[m]==2) {
          df$D_rampup[l] = as.numeric(difftime(df$datetime[m], df$datetime[l]))
          df$Ramp_up[l] = abs(df$parameter_value[l]-df$parameter_value[m])/as.numeric(difftime(df$datetime[m], df$datetime[l]))
          df$Strange_up[l] = abs(df$parameter_value[l]-df$parameter_value[m])/(df$ann_thre[1]+1) #to avoid zero
          df$RB_Index_up[l] = abs(df$parameter_value[l]-df$parameter_value[m])/((df$ann_thre[1]+1)*as.numeric(difftime(df$datetime[m], df$datetime[l])))}

        if (df$dgtag[l]==4 & df$dgtag[m]==3) {
          df$D_rampup[l] = as.numeric(difftime(df$datetime[m], df$datetime[l]))
          df$Ramp_up[l] = abs(df$parameter_value[l]-df$parameter_value[m])/as.numeric(difftime(df$datetime[m], df$datetime[l]))
          df$Strange_up[l] = abs(df$parameter_value[l]-df$parameter_value[m])/(df$ann_thre[1]+1) #to avoid zero
          df$RB_Index_up[l] = abs(df$parameter_value[l]-df$parameter_value[m])/((df$ann_thre[1]+1)*as.numeric(difftime(df$datetime[m], df$datetime[l])))}

        if (df$dgtag[l]==1 & df$dgtag[m]==3) {
          df$D_rampup[l] = as.numeric(difftime(df$datetime[m], df$datetime[l]))
          df$Ramp_up[l] = abs(df$parameter_value[l]-df$parameter_value[m])/as.numeric(difftime(df$datetime[m], df$datetime[l]))
          df$Strange_up[l] = abs(df$parameter_value[l]-df$parameter_value[m])/(df$ann_thre[1]+1) #to avoid zero
          df$RB_Index_up[l] = abs(df$parameter_value[l]-df$parameter_value[m])/((df$ann_thre[1]+1)*as.numeric(difftime(df$datetime[m], df$datetime[l])))}

        if (df$dgtag[l]==1 & df$dgtag[m]==2) {
          df$D_rampup[l] = as.numeric(difftime(df$datetime[m], df$datetime[l]))
          df$Ramp_up[l] = abs(df$parameter_value[l]-df$parameter_value[m])/as.numeric(difftime(df$datetime[m], df$datetime[l]))
          df$Strange_up[l] = abs(df$parameter_value[l]-df$parameter_value[m])/(df$ann_thre[1]+1) #to avoid zero
          df$RB_Index_up[l] = abs(df$parameter_value[l]-df$parameter_value[m])/((df$ann_thre[1]+1)*as.numeric(difftime(df$datetime[m], df$datetime[l])))}

        if (df$dgtag[l]==3 & df$dgtag[m]==1) {
          df$D_rampdw[l] = as.numeric(difftime(df$datetime[m], df$datetime[l]))
          df$Ramp_dw[l] = abs(df$parameter_value[l]-df$parameter_value[m])/as.numeric(difftime(df$datetime[m], df$datetime[l]))
          df$Strange_dw[l] = abs(df$parameter_value[l]-df$parameter_value[m])/(df$ann_thre[1]+1) #to avoid zero
          df$RB_Index_dw[l] = abs(df$parameter_value[l]-df$parameter_value[m])/((df$ann_thre[1]+1)*as.numeric(difftime(df$datetime[m], df$datetime[l])))}

        if (df$dgtag[l]==3 & df$dgtag[m]==2) {
          df$D_rampdw[l] = as.numeric(difftime(df$datetime[m], df$datetime[l]))
          df$Ramp_dw[l] = abs(df$parameter_value[l]-df$parameter_value[m])/as.numeric(difftime(df$datetime[m], df$datetime[l]))
          df$Strange_dw[l] = abs(df$parameter_value[l]-df$parameter_value[m])/(df$ann_thre[1]+1)#to avoid zero
          df$RB_Index_dw[l] = abs(df$parameter_value[l]-df$parameter_value[m])/((df$ann_thre[1]+1)*as.numeric(difftime(df$datetime[m], df$datetime[l])))}


        df$pk_rtn <- ifelse(df$pk_rtn > 12, NA, df$pk_rtn)
        df$offpk_rtn <- ifelse(df$offpk_rtn > 24, NA, df$offpk_rtn)
        df$D_rampup <- ifelse(df$D_rampup > 12, NA, df$D_rampup)
        df$D_rampdw <- ifelse(df$D_rampdw > 12, NA, df$D_rampdw)
        df$Ramp_dw <- ifelse(df$Ramp_dw < 5, NA, df$Ramp_dw)

      }}}
  return(df)
}

#' @export
HPK_rt_dur <- function(df) {

  if (length(stats::na.omit(df$dgtag))>2) {

    df <-  HPK_ramprt_duration(df)
    df <- df[,c("location_id","datetime", "parameter_value","dgtag","pk_rtn", "offpk_rtn", "D_rampup", "D_rampdw", "RB_Index_up", "RB_Index_dw", "Ramp_up", "Ramp_dw","Strange_up", "Strange_dw")]
  }
  return(df)
}
