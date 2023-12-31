% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convertTRACCS.R
\name{convertTRACCS}
\alias{convertTRACCS}
\title{Convert TRACCS road transportation data to iso country.}
\usage{
convertTRACCS(x, subtype)
}
\arguments{
\item{x}{a magpie data object}

\item{subtype}{One of the possible subtypes, see default argument.}
}
\value{
magpie object
}
\description{
Convert TRACCS road transportation data to iso country.
}
\examples{
\dontrun{
a <- readSource("TRACCS")
}
}
\seealso{
\code{\link{readSource}}
}
\author{
Johanna Hoppe, Alois Dirnaichner
}
