
HPK_Count_Pre <- function(df, alpha1, gamma){
  ## get the datetime a timeformat
  df$datetime <- ymd_hms(df$datetime)
  ## replace the NA value of dift_dis with 0.
  df <- df %>% mutate(parameter_value = ifelse(is.na(.data$parameter_value), 0, .data$parameter_value))

  ## cut the head and foot to get rid of small fluctuations
  y <- df %>%
    mutate(flow_90th = stats::quantile(.data$parameter_value, probs = 0.9)) %>%
    mutate(flow_10th = stats::quantile(.data$parameter_value, probs = 0.1)) %>%
    mutate(parameter_value = ifelse(.data$parameter_value < .data$flow_90th, .data$parameter_value, .data$flow_90th)) %>%
    mutate(parameter_value = ifelse(.data$parameter_value > .data$flow_10th, .data$parameter_value, .data$flow_10th)) %>%
    mutate(dift_dis = lead(.data$parameter_value,1) - .data$parameter_value) %>%
    ungroup()

  ## replace the NA value of dift_dis with 0.
  y <- y %>% mutate(dift_dis = ifelse(is.na(.data$dift_dis), 0, .data$dift_dis))

  ## replace the small value of dift_dis with 0.
  y <- y %>% mutate(dift_dis = ifelse(abs(.data$dift_dis) < gamma, 0, .data$dift_dis))

  ## replace didft_dis < ann_thre*0.03, 0.03 was chosen based on the dataset.
  y <- y %>%
    mutate(dift_dis = ifelse(abs(.data$dift_dis) < .data$ann_thre*alpha1, 0, .data$dift_dis)) %>%
    ungroup()

  return(y)
}

## Update the discharge based on the adjusted dift_dis, df is list
Q_adj_bydift <- function(df){

  n = nrow(df)-1

  for(i in 1:n){
    if(df$dift_dis[i]==0 & df$parameter_value[i] != df$parameter_value[i+1]){
      df$parameter_value[i+1] = df$parameter_value[i]
    }
  }

  return(df)
}

###-----------------------------------Alternative method -- vector angle to capture the reversal pts ----------------------##
##------------------------------------------- step 2.2  tag the data by time per year ----------------------------------------------##
add_tag.byYr <- function(df){ #this is included in the vector angle function)
  n = nrow(df)
  df$yr <- year(df$datetime)
  df$time_step <- rep(NA, nrow(df))
  unique_yr = unique(df$yr)
  t=1
  l=1

  for (i in 1:n) {
    if (df$yr[i] == unique_yr[l]) {
      df$time_step[i] <- t
      t <- t+1
    }else{
      t <- 1
      df$time_step[i] <- t
      t <- t+1
      l=l+1}}
  df <- df %>% group_by(.data$yr) %>% mutate(time_no = lead(.data$time_step) - .data$time_step) %>% ungroup() # at the end of ech year,I will have an NA

  return(df)
}

##-------------------------------------------step2.3 get the degree of vector -----------------------------------------------------##
vector_angle <- function(df){# this is also included in the next function
  n = nrow(df)
  time_no = df$time_no
  y = df$dift_dis
  vt_degree = rep(NA, n)
  unique_yr = unique(df$yr)
  i=1

  for (l in 2:n) {
    if (df$yr[l]==unique_yr[i]) {
      cosa = (time_no[l-1]*time_no[l] + y[l-1]*y[l])/sqrt((time_no[l-1]^2 + y[l-1]^2)*(time_no[l]^2 + y[l]^2))
      cosa = (pmin(pmax(cosa,-1.0),1.0))
      vt_degree[l] = acos(cosa)/pi*180
    }else{
      i=i+1
    }
  }
  df = cbind(df, vt_degree) #  At the end of each year, I will have 2NA
  return(df)
}

##-------------------------------------step2.4 divide pks into up and down process ---------------------------------------------##
# this one includes the above 2 functions
hpk_up_dw <- function(df, theta){

  n = nrow(df)

  # differentiate the up and dw pk pts, 2-start of station, 3-end of station
  df$dgtag <- ifelse(df$vt_degree>theta, ifelse(df$dift_dis == 0, 2 , ifelse(df$dift_dis<0, 3, 1)), 0)

  # find the start point of up
  for (i in 2:n) {
    if (df$dgtag[i] == 1 & df$dift_dis[i-1] ==0 & !is.na(df$vt_degree[i])) {
      df$dgtag[i] = 4#the start of up
    }
  }

  return(df)
}

#' @export
ReversalCount <- function(df, alpha1 = 0.03, theta = 60, gamma = 1.1){
  df <- HPK_Count_Pre(df, alpha1, gamma)
  df <- Q_adj_bydift(df)
  df <- add_tag.byYr(df)
  df <- vector_angle(df)
  df <- hpk_up_dw(df, theta)
  df <- df[, c("location_id","datetime","parameter_value","ann_thre","vt_degree","dgtag")]
  return(df)}
