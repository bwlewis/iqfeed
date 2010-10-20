\name{iqfeed-package}
\alias{iqfeed-package}
\alias{iqfeed}
\docType{package}
\title{
An R interface to DTN IQFeed.
}
\description{
The iqfeed package contains several convenience functions that access
IQFeed historic and other data from the DTN IQfeed product.
}
\details{
The iqfeed package supports basic historic and options chain data lookup
from IQFeed. We also include some functions for converting between 
various options symbol formats. (Those functions will fortunately become
obsolete when options symbology is standardized later in 2010.)

}
\author{
B. W. Lewis <blewis@illposed.net> with kind assistance from Greg Beckham.
}
\references{
http://www.iqfeed.net
}
\examples{
require(iqfeed)
iqConf()
t0 <- "2010-01-18 15:25:00"
t1 <- "2010-01-22 10:00:00"
trades <- HIT("MSFT",interval=60,start=t0,end=t1)
trades

daily <- HDX("XOM",days=14)
daily

# Options chain lookup example
symbols <- CEO("MSFT")
symbols

# Convert to standard OSI format (e.g., for use by a broker), and back:
osi(symbols[1])
osi2iq(.Last.value)

}
