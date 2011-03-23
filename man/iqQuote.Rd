\name{iqQuote}
\alias{iqQuote}
\title{
Quick and dirty real time quote
}
\description{
Retrieve a single real time quote from an IQFeed stream using a new
ephemeral IQFeed Level1 data session.
}
\usage{
iqQuote(symbol, type="P", limit=10)
}
\arguments{
  \item{symbol}{
An underlying equity symbol.
}
  \item{type}{
A character indicating the quote type (the first field in the IQFeed quote
format, usually "P" for price quote or "F" for fundamentals quote).
}
  \item{limit}{
Limit feed session to the number of lines specified.
}
}
\value{
A character vector containing the requested IQFeed quote data, or NULL if
no quote matching the input parameters was returned.
}
\author{
B. W. Lewis
}

\examples{
require('iqfeed')
iqConf()
iqQuote("MSFT")
}
