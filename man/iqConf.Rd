\name{iqConf}
\alias{iqConf}
\title{
Configure IQFeed connection
}
\description{
Configure the host and port numbers for an IQFeed connection.
}
\usage{
iqConf(host = "localhost", ports = list(level1=5009,historic=9100,level2=9200))
}
\arguments{
  \item{host}{
The host name or (character) I.P. address of the IQFeed.
}
  \item{ports}{
The IQFeed real-time, historic Level I, and historic Level II data ports. They
must be named 'level1' 'historic' and 'level2'!
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
