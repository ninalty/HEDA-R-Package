#' @import dplyr ggplot2 lubridate zoo rlang

#' @export
HEDA_Tidy <- function(df, season) {
  colnames(df) <- c("location_id", "datetime","parameter_value")

  df$mth <- month(df$datetime)
  df$yr <- year(df$datetime)
  df$dy <- day(df$datetime)
  df$nhr <- hour(df$datetime)

  # format the datetime
  df$datetime <- paste(df$yr, df$mth, df$dy, sep = "-")

  df$datetime <- paste(df$datetime, " ",df$nhr, ":00:00", sep = "")

  # get rid of negative value
  df$parameter_value <- ifelse(df$parameter_value < 0, 0, df$parameter_value)

  # get annual threshold
  df$ann_thre <- mean(df$parameter_value, na.rm = TRUE)

  df <- df[df$mth %in% season,]


  # check whether data is empty or not
  if (nrow(df)>10) {

    # Interpolation data

    ########################################### deal with NA data ######################################################

    ## Get out all the data that have NA
    kk_ <- df[is.na(df$parameter_value),]

    ## Count how many NA occurs per year per location_id
    kkt <- kk_ %>% group_by(.data$yr, .data$mth) %>% count()

    # for n > 720, that mth should be removed.
    kkt_720 <- kkt %>% filter(n >= 720)

    # remove these record
    kk <- anti_join(df,kkt_720, by=c("yr", "mth"))

    #################################################### Interpolation ##################################################
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
    kk <- kk %>% group_by(.data$location_id, .data$datetime) %>% mutate(parameter_value = mean(.data$parameter_value)) %>% distinct(.data$parameter_value, .data$ann_thre) %>% ungroup()

  }
  return(kk)
}
