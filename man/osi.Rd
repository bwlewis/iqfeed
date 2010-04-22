\name{osi}
\alias{osi}
\title{
Convert from IQFeed to OSI standard option symbol
}
\description{
Convert an older-style symbol to the standard OSI format.
}
\usage{
osi(symbol)
}
\arguments{
  \item{symbol}{
An older-style option symbol.
}
}
\details{
As of April 2010, IQFeed still uses older-syle option symbols for some
instruments. The OSI symbols may be used by all brokers to place orders.
}
\value{
A character representing the OSI symbol.
}
\references{
http://www.optionsclearing.com/initiatives/symbology/
}
\author{
B. W. Lewis
}
\note{
IQFeed will abondon older-style symbols at some point in 2010, making this
function obsolete.
}
\seealso{
{\code{\link{osi2iq}}}
}
\examples{
require(iqfeed)
iqConf()
# Options chain lookup example
symbols <- CEO("MSFT")
symbols

# Convert to standard OSI format (e.g., for use by a broker), and back:
osi(symbols[1])
osi2iq(.Last.value)
}
