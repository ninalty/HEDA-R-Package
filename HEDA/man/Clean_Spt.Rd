\name{Clean_Spt}
\alias{Clean_Spt}

\title{
Clean_Spt
}
\description{
Clean repeated points
}
\usage{
Clean_Spt(df, alpha3 = 0.7, alpha4 = 0.5)
}

\arguments{
  \item{df}{
Dataframe to be processed
}
  \item{alpha3}{
Default value: 0.7
}
  \item{alpha4}{
Default value: 0.5
}
}
\details{
Evaluate whether the difference in discharge between peaking and off-peaking points is qualified to be identified as hydropeaking events
}
\value{
Output file will be a dataframe in the same form as the input
}

\examples{
# before running the function
HPK_SampleData$dateTime <- parse_date_time(HPK_SampleData$dateTime,"mdy HM")

hpk_flow_cln <- HEDA_Tidy(HPK_SampleData, season = c(6,7,8,9))

hpk_flow_cg <- ReversalCount(hpk_flow_cln)
hpk_flow_cg <- Clean_position(hpk_flow_cg)

# clean continuous points of the same type
hpk_flow_cg <- Clean_Spt(hpk_flow_cg)

# or change the default values
hpk_flow_cg <- Clean_Spt(hpk_flow_cg, alpha3 = 0.7, alpha4 = 0.5)
}

