\name{crapmod}
\alias{crapmod}
\title{
Simple xts plot
}
\description{
Simple xts time-series plot in the style of quantmod.
}
\usage{
crapmod(x, title = "", col = 4, lwd = 2, lty = 1)
}
\arguments{
  \item{x}{An xts time-series object to plot.}
  \item{title}{The plot title.}
  \item{col}{The plot color.}
  \item{lwd}{The plot line width.}
  \item{lty}{The plot line type.}
}
\value{
Graphical side-effects.
}
\references{
http://www.optionsclearing.com/initiatives/symbology/
}
\author{
B. W. Lewis
}
\examples{
require(iqfeed)
iqConf()
x <- HIT("MSFT",interval=60,start=format(Sys.Date()-5,"\%Y-\%m-\%d"),beginFilterTime="092900",endFilterTime="155900")
crapmod(x[,"Close"], title="MSFT Close")
}
