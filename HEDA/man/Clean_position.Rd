\name{Clean_position}
\alias{Clean_position}

\title{
Clean_position
}
\description{
Change points are excluded if they are in the wrong position. For example, both point 3 and the peak pair represent the peaking discharge whose value (position) should be close to the daily maximum discharge. If the peaking discharge is close to the daily minimum discharge, change points will be removed since they are in the wrong positions
}
\usage{
Clean_position(df, alpha2 = 0.3)
}

\arguments{
  \item{df}{
dataframe to be processed
}
  \item{alpha2}{
Default value : 0.3
}
}

\value{
output will be a dataframe in the same form as the input
}

\examples{
# before running the function
HPK_SampleData$dateTime <- parse_date_time(HPK_SampleData$dateTime,"mdy HM")

hpk_flow_cln <- HEDA_Tidy(HPK_SampleData, season = c(6,7,8,9))

hpk_flow_cg <- ReversalCount(hpk_flow_cln)

# running the function to clean unqualified change points
hpk_flow_cg <- Clean_position(hpk_flow_cg)

# default value is open to change
hpk_flow_cg <- Clean_position(hpk_flow_cg, alpha2 = 0.3)
}
