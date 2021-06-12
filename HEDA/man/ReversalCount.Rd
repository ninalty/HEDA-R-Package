\name{ReversalCount}
\alias{ReversalCount}

\title{
ReversalCount
}
\description{
Detect hydropeaking events
}
\usage{
ReversalCount(df, alpha1 = 0.03, theta = 60, gamma = 1.1)
}

\arguments{
  \item{df}{
Dataframe to be processed
}
  \item{alpha1}{
Default value : 0.03
}
  \item{theta}{
Default value : 60 degree for m^3/s, 85 degree for cfs.
}
  \item{gamma}{
gamma default value: 1.1 m/s or 40 cfs.
}
}

\value{
Output dataframe has six columns containing location id, datatime, parameter value, ann_thre, vt_degree and dgtag. ann_thre is originally the mean annual discharge. If flow record is subset by season, ann_thre will be the mean discharge of that period. vt_degree is the vector angle between two flow vectors. dgtag is the catogrized change points.
}
\references{
Li, T. and Pasternack, G.B., 2021. Revealing the diversity of hydropeaking flow regimes. Journal of Hydrology, 598, p.126392.
}


\examples{
\dontrun{
hpk_flow_cg <- ReversalCount(hpk_flow_cln)

hpk_flow_cg <- ReversalCount(hpk_flow_cln, alpha1 = 0.03, theta = 85, gamma = 40)
}}
