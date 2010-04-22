\name{CEO}
\alias{CEO}
\title{
Retrieve an equity options symbol chain.
}
\description{
Retrieve the IQFeed options symbols for the indicated underlying equity.
}
\usage{
CEO(symbol, pc = "pc", monthCodes = "", near = 1)
}
\arguments{
  \item{symbol}{
An underlying equity symbol.
}
  \item{pc}{
A character: "p" for put options, "c" for call options, "pc" for both kinds.
}
  \item{monthCodes}{
Ignored if the near options is used. Otherwise, IQfeed month code (A-L for
January-December call options, and M-X for Jan-Dec put options).
}
  \item{near}{
Specify the number of months to cover (1 for options expiration within the next
month, 2 within the next two months, etc.).
}
}
\value{
A character vector containing the available IQFeed options symbols. Note that,
as of April 2010, IQFeed is using an old symbol format for many equity options.
See the \code{osi} function to convert to OSI standard symbology.
}
\author{
B. W. Lewis
}

\seealso{
\ref{\code{osi}}
}
\examples{
require('iqfeed')
iqConf()
CEO("MSFT")
}
