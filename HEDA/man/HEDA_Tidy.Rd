\name{HEDA_Tidy}
\alias{HEDA_Tidy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
HEDA_Tidy
}
\description{
Format flow record into hourly record; Split record by designated season; Interpolate and smooth the record.
}
\usage{
HEDA_Tidy(df, season)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Dataframe to be processed
}
  \item{season}{
Subset data by season
}
}

\value{
%%  ~Describe the value returned
Output dataframe has 4 columns containing location id, datatime, parameter value, ann_thre. ann_thre is originally the mean annual discharge. If flow record is subset by season, ann_thre will be the mean discharge during the subset period
}

\examples{

\dontrun{
# format the time column to time column
HPK_Sample_data$dateTime <- parse_date_time(HPK_Sample_data$dateTime,"mdy HM")

# clean subset data by season
hpk_flow_cln <- HEDA_Tidy(HPK_Sample_data, season = c(6,7,8,9))
}}
