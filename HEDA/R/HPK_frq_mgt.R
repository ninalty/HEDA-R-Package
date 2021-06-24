#Identify the rise and fall start and end point
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
        df$dw[m]<-"dw"}}}

  return(df)}

PK_time <- function(df){
  df = table(df)
  df = data.frame(df/sum(df)*100)
  names(df) <- c("Var","Freq")
  df$Var <- as.numeric(levels(df$Var))
  df$Freq <- as.numeric(df$Freq)
  df <- with(df, sum(Var*Freq/100))
  return(df)}

# count pk_no
up_dw_count <- function(df){

  up_no <- df %>% group_by(.data$datetime) %>% filter(.data$up=="up0") %>% count()
  dw_no <- df %>% group_by(.data$datetime) %>% filter(.data$dw=="dw0") %>% count()

  daily_pk_no <- full_join(up_no,dw_no, by="datetime")

  # fix the na value of n
  daily_pk_no$n.x <- ifelse(is.na(daily_pk_no$n.x), 0, daily_pk_no$n.x)
  daily_pk_no$n.y <- ifelse(is.na(daily_pk_no$n.y), 0, daily_pk_no$n.y)
  daily_pk_no$yr <- year(daily_pk_no$datetime)

  #add up and dw together by day
  daily_pk_no$pk_no <- (daily_pk_no$n.x + daily_pk_no$n.y) #not devided by 2

  return(daily_pk_no)}


#calculate the pkratio
PK_Ratio <- function(df1, df2){

  row1 <- nrow(df1)
  row2 <- nrow(df2)
  row_no <- ifelse(row1!=0, row2*24/row1*100, 0)

  return(row_no)
}



# ---------------------The exported function-----------------
#' @export
HPK_frq_mgt <- function(df) {

  df <- df %>% mutate(nhour = hour(.data$datetime)) %>% mutate(datetime = as.Date(.data$datetime))

  #Identify the rise and fall process of hydropeaking
  df <- up_dw_ID(df)

  if (length(stats::na.omit(df$up))>2) {

    # pk_no & pkratio
    HPK_updw_Daily <- up_dw_count(df)
    HPK_updw_Daily$location_id <- df$location_id[1]

    HPK_updw_Daily$pkratio <- PK_Ratio(df,HPK_updw_Daily)

    # peaking Q
    Q_pk_max <-  rbind(df[which(df$up=="up" & df$dgtag==2),], df[which(df$up=="up" & df$dgtag==3),])
    Q_pk_max$parameter_value <- Q_pk_max$parameter_value/Q_pk_max$ann_thre[1]
    colnames(Q_pk_max)[3] <- "Qpeak"

    # peaking timing
    Q_pk_max$Tmax <- PK_time(Q_pk_max$nhour)

    # off-peaking Q
    Q_pk_min <- rbind(df[which(df$dw=="dw" & df$dgtag==2),], df[which(df$up=="dw" & df$dgtag==1),])
    Q_pk_min$parameter_value <- Q_pk_min$parameter_value/Q_pk_min$ann_thre[1]
    colnames(Q_pk_min)[3] <- "offQpeak"

    #merge then into one data frame
    hpk_frq_mg = left_join(HPK_updw_Daily, Q_pk_max, by = "datetime")
    hpk_frq_mg = left_join(hpk_frq_mg, Q_pk_min, by = "datetime")
    hpk_frq_mg =  hpk_frq_mg[, c("location_id", "datetime", "Qpeak", "offQpeak","pk_no","pkratio")]
    hpk_frq_mg$location_id <- unique(stats::na.omit(hpk_frq_mg$location_id))
  }
  return(hpk_frq_mg)}
