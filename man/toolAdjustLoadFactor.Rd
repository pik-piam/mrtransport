% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toolAdjustLoadFactor.R
\name{toolAdjustLoadFactor}
\alias{toolAdjustLoadFactor}
\title{Perform parameter specific adjustments on the input data}
\usage{
toolAdjustLoadFactor(dt, completeData, TRACCScountries, filter)
}
\arguments{
\item{dt}{calculated raw data without adjustments}

\item{completeData}{All combinations of region, period, univocalName and technology in EDGE-T decision tree}

\item{TRACCScountries}{countries included in TRACCS database}

\item{filter}{list of filters for specific branches in the upper decision tree, containing all associated
univocalNames}
}
\value{
a quitte object
}
\description{
Perform parameter specific adjustments on the input data
}
\author{
Johanna Hoppe
}
