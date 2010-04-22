\name{osi2iq}
\alias{osi2iq}
\title{
Convert from an OSI standard symbol to the IQFeed symbol format.
}
\description{
Convert from an OSI standard symbol to the IQFeed symbol format.
}
\usage{
osi2iq(symbol)
}
\arguments{
  \item{symbol}{
An OSI symbol.
}
}
\value{
(Character) old-style IQFeed symbol.
}
\author{
B. W. Lewis
}
\note{
IQFeed will abondon older-style symbols at some point in 2010, making this
function obsolete.
}
\seealso{
\ref{\code{osi}}
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
