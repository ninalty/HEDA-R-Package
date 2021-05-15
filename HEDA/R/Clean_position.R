#' @export
Clean_position <- function(df, alpha2 = 0.3){

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

  #clean by the dynamic threshold
  index_lt <- which(df$dgtag %in% c(1,2,3,4))

  n <- length(index_lt)-1

  a=1
  if (length(index_lt)>1) {
    for(i in 1:n){
      x1 = index_lt[i]
      x2 = index_lt[i+1]

      if(x1 > 24*a){
        a=a+1}
      #-- pt#1
      if(df$dgtag[x1]==1 & df$parameter_value[x1] > Q24.max[a]-Q24.delta[a]*alpha2){#low pt shouldn't be too close to the high pt
        df$dgtag[x1] <- 0}

      #--pt3
      if(df$dgtag[x2]==3 & df$dgtag[x1]!=2 & df$parameter_value[x2] < Q24.min[a]+Q24.delta[a]*alpha2){#high pt shouldn't be close to low pt
        df$dgtag[x2] <- 0}

      #--pt23
      if(df$dgtag[x1]==2 & df$dgtag[x2]==3 & df$parameter_value[x1] < Q24.max[a]-Q24.delta[a]*alpha2){#shouldn't be low
        df$dgtag[x1] <- 0
        df$dgtag[x2] <- 0}

      #--pt24
      if(df$dgtag[x1]==2 & df$dgtag[x2]==4 & df$parameter_value[x1] > Q24.min[a]+Q24.delta[a]*alpha2){#shouldn't be high
        df$dgtag[x1] <- 0
        df$dgtag[x2] <- 0}}}
  return(df)}
