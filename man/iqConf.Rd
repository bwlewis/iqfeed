\name{iqConf}
\alias{iqConf}
\title{
Configure IQFeed connection
}
\description{
Configure the host and port numbers for an IQFeed connection.
}
\usage{
iqConf(host = "localhost", ports = list(5009, 9100, 9200))
}
\arguments{
  \item{host}{
The host name or (character) I.P. address of the IQFeed.
}
  \item{ports}{
The IQFeed real-time, historic Level I, and historic Level II data ports (in that order!).
}
}
\details{
Connections to the feed are generally not held open. The iqConf function may be run at any time to re-configure port numbers. The default parameter values are the default IQFeed port numbers.
}
\value{
Nothing is returned.
}
\author{
B. W. Lewis <blewis@illposed.net>
}
\examples{
require('iqfeed')
iqConf()
}
