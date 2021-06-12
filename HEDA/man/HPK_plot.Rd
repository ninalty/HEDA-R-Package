\name{HPK_plot}
\alias{HPK_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
HPK_plot
}
\description{
Plot hydrograph
}
\usage{
HPK_plot(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Dataframe to be processed
}
}
\details{
Plot the hydrograph of the processed data with change points marked by different colors
}
\value{
The plot will be presented under the “Plots” tab in RStudio. Users can determine whether to save out thediagram by themselves. The plot shows flow in m^3/s
}

\examples{
\dontrun{
## subset the data
tt = hpk_flow_cg[13100:13400,]

## plot the hydrograph of the subset data
HPK_plot(tt)
}}

