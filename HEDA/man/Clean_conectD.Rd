\name{Clean_conectD}
\alias{Clean_conectD}

\title{
Clean_conectD
}
\description{
Evaluate difference between peaking and off-peaking discharge
}
\usage{
Clean_conectD(df, alpha3 = 0.7, alpha4 = 0.5)
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
Evaluate whether the difference in discharge between peaking and off-peaking points is qualified to be identified as hydropeaking events.
}
\value{
The output will be a dataframe in the same form with the input.
}

\examples{
\dontrun{
# before running the function
HPK_SampleData$dateTime <- parse_date_time(HPK_SampleData$dateTime,"mdy HM")

hpk_flow_cln <- HEDA_Tidy(HPK_SampleData, season = c(6,7,8,9))

hpk_flow_cg <- ReversalCount(hpk_flow_cln)
hpk_flow_cg <- Clean_position(hpk_flow_cg)
hpk_flow_cg <- Clean_Spt(hpk_flow_cg)

# clean the unqualified peaking and off-peaking discharge
hpk_flow_cg <- Clean_conectD(hpk_flow_cg)

# or change the default values
hpk_flow_cg <- Clean_conectD(hpk_flow_cg, alpha3 = 0.7, alpha4 = 0.5)
}}
