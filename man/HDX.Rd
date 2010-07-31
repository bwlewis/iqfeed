\name{HDX}
\alias{HDX}
\title{
Retrieves daily historic bars for a given number of days, for the
specified symbol.
}
\description{
Retrieves daily historic bars for a given number of days, for the
specified symbol.
}
\usage{
HDX(symbol, days, tz="")
}
\arguments{
  \item{symbol}{
Any valid symbol.
}
  \item{days}{
The number of days.
}
  \item{tz}{
The timezone code (defaults to "", or the current timezone, as timezone translation is not typically desired in this context).
}
}
\value{
An xts time-series object with High, Low, Open, Close, and Volume bars
over the requested interval.
}
\author{
Joseph Dunn
}

\examples{
require("iqfeed")
iqConf()
HDX("XOM",days=14)
}
