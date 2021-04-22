
library(dplyr)
library(lubridate)
library(zoo)

# coefficients used in thresholds
aerfa1 = 0.03  #small fluctuations in predata process
aerfa2 = 0.3  # position layer
aerfa3 = 0.7   # T3 Local static Qave
aerfa4 = 0.5  # T3 dynamic
theta = 60     # vector angle



HPK_Count_Pre <- function(x, aerfa1){ #X is dataframe
  # get the datetime a timeformat
  x$datetime <- ymd_hms(x$datetime)
  ## replace the NA value of dift_dis with 0.
  x <- x %>% mutate(parameter_value = ifelse(is.na(parameter_value), 0, parameter_value))

  # cut the head and foot to get rid of small fluctuations
  y <- x %>%
    mutate(flow_80th = quantile(parameter_value, probs = 0.9)) %>%
    mutate(flow_20th = quantile(parameter_value, probs = 0.1)) %>%
    mutate(parameter_value = ifelse(parameter_value < flow_80th, parameter_value, flow_80th)) %>%
    mutate(parameter_value = ifelse(parameter_value > flow_20th, parameter_value, flow_20th)) %>%
    mutate(dift_dis = lead(parameter_value,1)-parameter_value) %>%
    ungroup()

  ## replace the NA value of dift_dis with 0.
  y <- y %>% mutate(dift_dis = ifelse(is.na(dift_dis), 0, dift_dis))

  ## replace the small value of dift_dis with 0.
  y <- y %>% mutate(dift_dis = ifelse(abs(dift_dis) < 20, 0, dift_dis))

  ##### replace didft_dis < ann_thre*0.03, 0.03 was chosen based on the dataset.
  y <- y %>%
    mutate(dift_dis = ifelse(abs(dift_dis) < ann_thre*aerfa1, 0, dift_dis)) %>%
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
hpk_up_dw <- function(df, theta){

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
ReversalCount <- function(df, aerfa1, theta){
  df <- HPK_Count_Pre(df, aerfa1)
  df <- Q_adj_bydift(df)
  df <- add_tag.byYr(df)
  df <- vector_angle(df)
  df <- hpk_up_dw(df, theta)
  return(df)}


##------------------------------------------------------- Step 3 clean the noise ------------------------------------------------##
###------------------------------------------------ Step 3.1 clean the point (based on the position) ----------------------------------------##


###--------------------------------------------------- Step3.2 clean points (remove the same pts) -----------------------##


#---------------------------------------------------  Step3.3 clean point (remove the continuouse pts of the same type) --------------------


###---------------------------------------------------put two function into one------------------------------------------------
# not sure whether used it

##--------------------------------------------------Repeat step 3.2 before Step3.4 clean point (remove the continuouse pts of the different type)--------------------------------------------------------------------


##--------------------------------------------------Repeat step 3.3 before Step3.4 clean point (remove the continuouse pts of the different type)----------------------------
clean_smfluc <- function(df){
  ## get the dynamic threshold
  Q24.max <- rollapply(df$parameter_value, 24, max, align="right", by=24)

  Q24.min <- rollapply(df$parameter_value, 24, min, align="right", by=24)

  Q24.delta <- Q24.max-Q24.min

  ## when nrow(df)/24 is not integer
  l_Q24 <- length(Q24.delta)

  if(nrow(df)/24 > nrow(df)%/%24){
    Q24.delta[l_Q24+1] <- Q24.delta[l_Q24]
    Q24.max[l_Q24+1] <- Q24.max[l_Q24]
    Q24.min[l_Q24+1] <- Q24.min[l_Q24]}

  # update the locataion of the reversal pts
  index_lt <- which(df$dgtag %in% c(1,2,3,4))
  n <- length(index_lt)-2
  a=1

  if (length(index_lt)>3) {
    for(i in 2:n){
      x1 = index_lt[i-1]
      x2 = index_lt[i]
      x3 = index_lt[i+1]
      x4 = index_lt[i+2]

      if(x2 > 24*a){
        a=a+1}

      #pt#1 -- pt#23
      if(df$dgtag[x1] ==1 & df$dgtag[x2]==2 & df$dgtag[x3]==3 & abs(df$parameter_value[x1]-df$parameter_value[x3]) < df$ann_thre[1]*0.1){
        if(df$parameter_value[x1] != Q24.min[a] & df$parameter_value[x2] != Q24.max[a]){
          df$dgtag[x1] <- 0
          df$dgtag[x2] <- 0
          df$dgtag[x3] <- 0}}

      #pt#1 -- pt#3
      if(df$dgtag[x1] ==1 & df$dgtag[x2]==3 & abs(df$parameter_value[x1]-df$parameter_value[x2]) < df$ann_thre[1]*0.1){
        if(df$parameter_value[x1] != Q24.min[a] & df$parameter_value[x2] != Q24.max[a]){
          df$dgtag[x1] <- 0
          df$dgtag[x2] <- 0}}

      #pt#3 --pt#1
      if(df$dgtag[x1] !=2  & df$dgtag[x2]==3 & df$dgtag[x3]==1 & abs(df$parameter_value[x2]-df$parameter_value[x3]) < df$ann_thre[1]*0.1){
        if(df$parameter_value[x3] != Q24.min[a] & df$parameter_value[x2] != Q24.max[a]){
          df$dgtag[x2] <- 0
          df$dgtag[x3] <- 0}}

      #pt#3 -- pt24
      if(df$dgtag[x1]!=2 & df$dgtag[x2] ==3 & df$dgtag[x3]==2 & df$dgtag[x4]==4 & abs(df$parameter_value[x2]-df$parameter_value[x3]) < df$ann_thre[1]*0.1){
        if(df$parameter_value[x3] != Q24.min[a] & df$parameter_value[x2] != Q24.max[a]){
          df$dgtag[x2] <- 0
          df$dgtag[x3] <- 0
          df$dgtag[x4] <- 0}}

      #pt23---pt#1
      if(df$dgtag[x1] ==2 & df$dgtag[x2]==3 & df$dgtag[x3]==1 & abs(df$parameter_value[x2]-df$parameter_value[x3]) < df$ann_thre[1]*0.1){
        if(df$parameter_value[x3] != Q24.min[a] & df$parameter_value[x2] != Q24.max[a]){
          df$dgtag[x1] <- 0
          df$dgtag[x2] <- 0
          df$dgtag[x3] <- 0}}

      #pt23--pt24
      if(df$dgtag[x1] ==2 & df$dgtag[x2]==3 & df$dgtag[x3]==2 & df$dgtag[x4]==4 & abs(df$parameter_value[x2]-df$parameter_value[x3]) < df$ann_thre[1]*0.1){
        if(df$parameter_value[x4] != Q24.min[a] & df$parameter_value[x2] != Q24.max[a]){
          df$dgtag[x1] <- 0
          df$dgtag[x2] <- 0
          df$dgtag[x3] <- 0
          df$dgtag[x4] <- 0}}

      #pt24--pt23
      if(df$dgtag[x1] ==2 & df$dgtag[x2]==4 & df$dgtag[x3]==2 & df$dgtag[x4]==3 & abs(df$parameter_value[x2]-df$parameter_value[x3]) < df$ann_thre[1]*0.1){
        if(df$parameter_value[x1] != Q24.min[a] & df$parameter_value[x3] != Q24.max[a]){
          df$dgtag[x1] <- 0
          df$dgtag[x2] <- 0
          df$dgtag[x3] <- 0
          df$dgtag[x4] <- 0}}

      #pt24--pt#3
      if(df$dgtag[x1]==2 & df$dgtag[x2] ==4 & df$dgtag[x3]==3 & abs(df$parameter_value[x2]-df$parameter_value[x3]) < df$ann_thre[1]*0.1){
        if(df$parameter_value[x1] != Q24.min[a] & df$parameter_value[x3] != Q24.max[a]){
          df$dgtag[x1] <- 0
          df$dgtag[x2] <- 0
          df$dgtag[x3] <- 0}}}}

  # print("Fluctuation cleaned!")
  return(df)}

