
library(dplyr)
library(lubridate)
library(zoo)

# coefficients used in thresholds
alpha1 = 0.03  #small fluctuations in predata process
alpha2 = 0.3  # position layer
alpha3 = 0.7   # T3 Local static Qave
alpha4 = 0.5  # T3 dynamic
theta = 85     # vector angle, 60 degree for m3/s; 85 degree for cfs



HPK_Count_Pre <- function(x, alpha1 = 0.03){ #X is dataframe
  # get the datetime a timeformat
  x$datetime <- ymd_hms(x$datetime)
  ## replace the NA value of dift_dis with 0.
  x <- x %>% mutate(parameter_value = ifelse(is.na(parameter_value), 0, parameter_value))

  # cut the head and foot to get rid of small fluctuations
  y <- x %>%
    mutate(flow_90th = quantile(parameter_value, probs = 0.9)) %>%
    mutate(flow_10th = quantile(parameter_value, probs = 0.1)) %>%
    mutate(parameter_value = ifelse(parameter_value < flow_90th, parameter_value, flow_90th)) %>%
    mutate(parameter_value = ifelse(parameter_value > flow_10th, parameter_value, flow_10th)) %>%
    mutate(dift_dis = lead(parameter_value,1)-parameter_value) %>%
    ungroup()

  ## replace the NA value of dift_dis with 0.
  y <- y %>% mutate(dift_dis = ifelse(is.na(dift_dis), 0, dift_dis))

  ## replace the small value of dift_dis with 0.
  y <- y %>% mutate(dift_dis = ifelse(abs(dift_dis) < 20, 0, dift_dis))

  ##### replace didft_dis < ann_thre*0.03, 0.03 was chosen based on the dataset.
  y <- y %>%
    mutate(dift_dis = ifelse(abs(dift_dis) < ann_thre*alpha1, 0, dift_dis)) %>%
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
add_tag.byYr <- function(df){#this is included in the vector angle function)
  n = nrow(df)
  df$yr <- year(df$datetime)
  df$tag_yr <- rep(NA, nrow(df))
  unique_yr = unique(df$yr)
  t=1
  l=1

  for (i in 1:n) {
    if (df$yr[i] == unique_yr[l]) {
      df$tag_yr[i] <- t
      t <- t+1
    }else{
      t <- 1
      df$tag_yr[i] <- t
      t <- t+1
      l=l+1}}
  df <- df %>% group_by(yr) %>% mutate(x=lead(tag_yr)-tag_yr) %>% ungroup()# at the end of ech year,I will have an NA

  return(df)
}

##-------------------------------------------step2.3 get the degree of vector -----------------------------------------------------##
vector_angle <- function(df){# this is also included in the next function
  n = nrow(df)
  x = df$x
  y = df$dift_dis
  degree = rep(NA, n)
  unique_yr = unique(df$yr)
  i=1

  for (l in 2:n) {
    if (df$yr[l]==unique_yr[i]) {
      cosa = (x[l-1]*x[l] + y[l-1]*y[l])/sqrt((x[l-1]^2 + y[l-1]^2)*(x[l]^2 + y[l]^2))
      cosa = (pmin(pmax(cosa,-1.0),1.0))
      degree[l] = acos(cosa)/pi*180
    }else{
      i=i+1
    }
  }
  df = cbind(df, degree) #  At the end of each year, I will have 2NA
  return(df)
}

##-------------------------------------step2.4 divide pks into up and down process ---------------------------------------------##
# this one includes the above 2 functions
hpk_up_dw <- function(df, theta = 60){

  n = nrow(df)

  #differentiate the up and dw pk pts, 2-start of station, 3-end of station
  df$dgtag <- ifelse(df$degree>theta, ifelse(df$dift_dis == 0, 2 , ifelse(df$dift_dis<0, 3, 1)), 0)

  #find the start point of up
  for (i in 2:n) {
    if (df$dgtag[i] == 1 & df$dift_dis[i-1] ==0 & !is.na(df$degree[i])) {
      df$dgtag[i] = 4#the start of up
    }
  }

  return(df)
}

#' @export
ReversalCount <- function(df, alpha1 = 0.03, theta = 60){
  df <- HPK_Count_Pre(df, alpha1)
  df <- Q_adj_bydift(df)
  df <- add_tag.byYr(df)
  df <- vector_angle(df)
  df <- hpk_up_dw(df, theta)
  return(df)}