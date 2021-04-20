#' @export
clean_Spt <- function(df, aerfa3, aerfa4){
  df <- clean_cotinpt(df, aerfa3, aerfa4)
  df <- clean_conectS(df, aerfa3, aerfa4)
  return(df)}

clean_cotinpt <- function(df, aerfa3, aerfa4){
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

  ## continuous pts
  # update the locataion of the reversal pts
  index_lt <- which(df$dgtag %in% c(1,2,3,4))
  n <- length(index_lt)-2
  a=1

  if(length(index_lt)>4){
    for(i in 2:n){
      x1 = index_lt[i-1]
      x2 = index_lt[i]
      x3 = index_lt[i+1]
      x4 = index_lt[i+2]

      if(x1 > 24*a){
        a=a+1}

      #continuouse pt#1
      if(df$dgtag[x1] ==1 & df$dgtag[x2] ==1){
        if(df$parameter_value[x1] <= df$parameter_value[x2]){
          df$dgtag[x1] <- 0}

        if(df$parameter_value[x1] > df$parameter_value[x2]){
          df$dgtag[x2] <- 0}}

      #continuouse pt#3
      #i the 3rd one is black, ignore the middle one
      if(df$dgtag[x1]==3 & df$dgtag[x2]==3 & df$dgtag[x3]==3){
        df$dgtag[x2] <- 0
        df$parameter_value[x2] <- (df$parameter_value[x2-1] + df$parameter_value[x2+1])/2}

      #the 3rd one is dift color, the value of the middle black is in the middle, ignore the middle black pts.
      if(df$dgtag[x2]==3 & df$dgtag[x1]!=2 & df$dgtag[x3]==3){
        if(df$parameter_value[x2] <= df$parameter_value[x3]){
          df$dgtag[x2] <-0
          df$parameter_value[x2] <- (df$parameter_value[x2-1] + df$parameter_value[x2+1])/2}

        if(df$parameter_value[x2] > df$parameter_value[x3]){
          df$dgtag[x3] <-0
          df$parameter_value[x3] <- (df$parameter_value[x3-1] + df$parameter_value[x3+1])/2}}

      #pair #23-23
      if(df$dgtag[x1]==2 & df$dgtag[x3]==2 & df$dgtag[x2]==3 & df$dgtag[x4]==3 & abs(df$parameter_value[x1]-df$parameter_value[x3])<Q24.delta[a]*0.01){# merge close pairs
        df$dgtag[x2] <- 0
        df$dgtag[x3] <- 0
        df$parameter_value[x1] <- max(df$parameter_value[x1], df$parameter_value[x3])
        df$parameter_value[x4] <- max(df$parameter_value[x1], df$parameter_value[x3])}

      if(df$dgtag[x1]==2 & df$dgtag[x3]==2 & df$dgtag[x2]==3 & df$dgtag[x4]==3 & abs(df$parameter_value[x1]-df$parameter_value[x3])<max(Q24.delta[a]*aerfa4,df$ann_thre[1]*aerfa3)){# remove not far enough pairs
        if (df$parameter_value[x3] > df$parameter_value[x1]) {
          df$dgtag[x1] <- 0
          df$dgtag[x2] <- 0}
        if(df$parameter_value[x3] < df$parameter_value[x1]){
          df$dgtag[x3] <- 0
          df$dgtag[x4] <- 0}}

      #pair 24-24
      if(df$dgtag[x1]==2 & df$dgtag[x3]==2 & df$dgtag[x2]==4 & df$dgtag[x4]==4 & abs(df$parameter_value[x1]-df$parameter_value[x3])<Q24.delta[a]*0.01){# merge close pairs
        df$dgtag[x2] <- 0
        df$dgtag[x3] <- 0
        df$parameter_value[x1] <- min(df$parameter_value[x1], df$parameter_value[x3])
        df$parameter_value[x4] <- min(df$parameter_value[x1], df$parameter_value[x3])}

      if(df$dgtag[x1]==2 & df$dgtag[x3]==2 & df$dgtag[x2]==4 & df$dgtag[x4]==4 & df$parameter_value[x1]<df$parameter_value[x3] & abs(df$parameter_value[x1]-df$parameter_value[x3])<max(Q24.delta[a]*aerfa4,df$ann_thre[1]*aerfa3)){#shred tends to be large
        df$dgtag[x3] <- 0
        df$dgtag[x4] <- 0}

      if(df$dgtag[x1]==2 & df$dgtag[x3]==2 & df$dgtag[x2]==4 & df$dgtag[x4]==4 & df$parameter_value[x1]>df$parameter_value[x3] & abs(df$parameter_value[x1]-df$parameter_value[x3])<max(Q24.delta[a]*aerfa4,df$ann_thre[1]*aerfa3)){#shred tends to be large
        df$dgtag[x1] <- 0
        df$dgtag[x2] <- 0}}}
  # print("Continupts cleaned!")
  return(df)}


clean_conectS <- function(df, aerfa3, aerfa4){

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

  ###continuous pts
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
      #compare with pair24-- similar type of pts
      #The former pt of pair pt1-->pt24
      if(df$dgtag[x2]==2 & df$dgtag[x3]==4 & df$dgtag[x4]==1 & abs(df$parameter_value[x2]-df$parameter_value[x4])<max(Q24.delta[a]*0.01)){#tend to be small
        df$dgtag[x4] <- 4
        df$parameter_value[x4] <- df$parameter_value[x3]
        df$dgtag[x3] <- 0}

      if(df$dgtag[x2]==2 & df$dgtag[x3]==4 & df$dgtag[x1]==1 & abs(df$parameter_value[x1]-df$parameter_value[x2])<max(Q24.delta[a]*0.01)){#tend to be small
        df$dgtag[x1] <- 2
        df$parameter_value[x1] <- df$parameter_value[x2]
        df$dgtag[x2] <- 0}

      if(df$dgtag[x2]==2 & df$dgtag[x3]==4 & df$dgtag[x1]==1 & abs(df$parameter_value[x1]-df$parameter_value[x3])<max(Q24.delta[a]*aerfa4,df$ann_thre[1]*aerfa3)){
        if(df$parameter_value[x1] < df$parameter_value[x2]) {
          df$dgtag[x2] <- 0
          df$dgtag[x3] <- 0}
        if(df$parameter_value[x1] >= df$parameter_value[x2]) {
          df$dgtag[x1] <- 0}}

      if(df$dgtag[x2]==2 & df$dgtag[x3]==4 & df$dgtag[x4]==1 & abs(df$parameter_value[x2]-df$parameter_value[x4])<max(Q24.delta[a]*aerfa4,df$ann_thre[1]*aerfa3)){
        if(df$parameter_value[x4] < df$parameter_value[x2]) {
          df$dgtag[x2] <- 0
          df$dgtag[x3] <- 0}
        if(df$parameter_value[x4] >= df$parameter_value[x2]){
          df$dgtag[x4] <- 0}}

      # pt#3 ~~~ pt#23
      #pt#3 --> 2&3 pair
      if(df$dgtag[x1]!=2 & df$dgtag[x2]==3 & df$dgtag[x3]==2 & df$dgtag[x4]==3 & abs(df$parameter_value[x2]-df$parameter_value[x3])<Q24.delta[a]*0.01){ #merge  close pt
        df$dgtag[x2] <- 2
        df$parameter_value[x2] <- df$parameter_value[x3]
        df$dgtag[x3] <- 0}

      #pt2&3 pair --> pt#3
      if(df$dgtag[x1]==2 & df$dgtag[x2]==3 & df$dgtag[x3]==3 & abs(df$parameter_value[x1]-df$parameter_value[x3])<Q24.delta[a]*0.01){#merge
        df$dgtag[x2] <- 0}

      #pt#3 --> 2&3 pair
      if(df$dgtag[x1]!=2 & df$dgtag[x2]==3 & df$dgtag[x3]==2 & df$dgtag[x4]==3 & abs(df$parameter_value[x2]-df$parameter_value[x3])<max(Q24.delta[a]*aerfa4,df$ann_thre[1]*aerfa3)){

        if (df$parameter_value[x2] <= df$parameter_value[x3]){
          df$dgtag[x2] <- 0}
        if(df$parameter_value[x2] > df$parameter_value[x3]){
          df$dgtag[x3] <- 0
          df$dgtag[x4] <- 0}}

      #pt2&3 pair --> pt#3
      if(df$dgtag[x1]==2 & df$dgtag[x2]==3 & df$dgtag[x3]==3 & abs(df$parameter_value[x1]-df$parameter_value[x3])<max(Q24.delta[a]*aerfa4,df$ann_thre[1]*aerfa3)){
        if (df$parameter_value[x1] < df$parameter_value[x3]) {
          df$dgtag[x1] <- 0
          df$dgtag[x2] <- 0}
        if (df$parameter_value[x1] >= df$parameter_value[x3]) {
          df$dgtag[x3] <- 0}}}}

  # print("ConnectS1 cleaned!")
  return(df)}
