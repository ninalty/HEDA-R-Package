\name{HPK_frq_mgt}
\alias{HPK_frq_mgt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
HPK_frq_mgt
}
\description{
Extract frequency and magnitude metrics
}
\usage{
HPK_frq_mgt(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Dataframe to be processed
}
}
\details{
Extract Qpeak, offQpeak, pk_no and pkraio from the identified hydropeaking events
}
\value{
Output dataframe includes time-series values of four metrics. Qpeak is the hydropeaking discharge, offQpeak isbase flow, pk_no is the daily number of rise and fall process, pkratio is the ratio of days with hydropeaking to thetotal number of days analyzed
}

\examples{
\dontrun{
HpkFrqMgt <- HPK_frq_mgt(hpk_flow_cg)
}}
