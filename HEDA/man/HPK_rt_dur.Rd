\name{HPK_rt_dur}
\alias{HPK_rt_dur}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
HPK_rt_dur
}
\description{
Extract rate of change and duration related metrics
}
\usage{
HPK_rt_dur(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Dataframe to be processed
}
}
\details{
Extract rate of chane and duration related metrics: pk_rtn is the retention of peaking process. offpk_rtn is theretention of base flow. D_rampup/D_ramp is the duration of rise/fall process. RB_Indx_up/RB_Index_dw is theflashness of rise/fall process. Ramp_up/Ramp_dw is the rate of change of rise/fall process.Strange_up/Strange_dw is the standardized rise/fall amplitude
}
\value{
Output dataframe includes time-series values of four metrics. Qpeak is the hydropeaking discharge, offQpeak isbase flow, pk_no is the daily number of rise and fall process, pkratio is the ratio of days with hydropeaking to thetotal number of days analyzed
}

\examples{
\dontrun{
HpkRtDur <- HPK_rt_dur(hpk_flow_cg)

## to extract the time-series of one metric withouth missing value
pk_rtn <- HpkRtDur[na.omit(HpkRtDur$pk_rtn),c("location_id","datetime","pk_rtn")]
}}

