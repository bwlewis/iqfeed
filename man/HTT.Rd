\name{HTT}
\alias{HTT}
\title{
Retrieves historic quotes within a given time interval for 
the specified symbol.
}
\description{
Retrieves historic quotes within a given time interval for 
the specified symbol.
}
\usage{
HTT(symbol, start, end, beginFilterTime = "", endFilterTime = "", tz="America/New_York")
}
\arguments{
  \item{symbol}{
Any valid symbol.
}
  \item{start}{
The starting date/time or just date.
}
  \item{end}{
The ending date/time or just date.
}
  \item{beginFilterTime}{
The beginning time of day (America/New_York) in the form "HHmmSS" (leave blank for all day).
}
  \item{endFilterTime}{
The ending time of day (America/New_York) in the form "HHmmSS" (leave blank for all day).
}
  \item{tz}{
The timezone code (defaults to "America/New_York").
}
}
\details{
A basic attempt is made to interpret the date/time values. One may
very explicitly specify the date/time parameters in the following
format: "CCCC-MM-dd HH:mm:SS" and by specifying the \code{tz} parameter.
}
\value{
An xts time-series object with quote data over the requested interval.
}
\author{
B. W. Lewis
}

\examples{
require("iqfeed")
iqConf()
t0 <- "2010-01-18 15:25:00"
HTT("MSFT",start=t0)
}
