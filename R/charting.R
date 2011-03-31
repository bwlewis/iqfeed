crapmod = function(x, title="", col=4, lwd=2, lty=1, xlim=NULL, ylim=NULL, ...)
{
  extraOptions = list(...)
  p = as.numeric(x[,1])
  plot.new()
  par(mar=c(2.2,3,3,3))
  plot(p, axes=FALSE, type='l', col="#eeeeee",ylab="Close", main=title, xlab='',
       xlim=xlim, ylim=ylim)
  box()
  yax = pretty(p)
  if(!is.null(ylim)) yax = pretty(ylim)
  axis(side=2,yax)
  axis(side=4,yax)
  at = endpoints(x,on="days") + 1 
  if(at[length(at)] > length(x))
    at = at[-length(at)]
  hat = endpoints(x,on="hours") + 1 
  if(hat[length(hat)] > length(x))
    hat = hat[-length(at)]
  mat = endpoints(x,on="months") + 1 
  if(mat[length(mat)] > length(x))
    mat = mat[-length(mat)]
  yat = endpoints(x,on="years") + 1 
  if(yat[length(yat)] > length(x))
    yat = yat[-length(yat)]
  abline(h=yax,lwd=1,col="#aabbff")
  if(length(mat)>1) {
# Plot a whole month or more of data
    M = format(index(x)[mat],"%h")
    axis(side=1,at=mat,labels=FALSE)
    abline(v=mat,lwd=1,col="#aabbff")
    if(length(yat)>0) {
      Y = format(index(x)[yat],"%Y")
      mtext(Y, side=1,at=yat,line=1,cex=1)
    }
    if(length(Y)<3) {
      mtext(M, side=1,at=mat,line=0,cex=0.75)
      axis(side=1,at=at,labels=FALSE,col="#555555",lwd.ticks=1)
    }
    lines(1:length(index(x)),x,col=col,lwd=lwd,lty=lty)
    return(invisible())
  }
  D = unique(format(index(x)[at],"%h %d"))
  D = D[!is.na(D)]
  ticks = extraOptions$ticks
  if(is.null(ticks)) ticks = 1:length(at)
  axis(side=1,at=at[ticks],labels=D[ticks])
  abline(v=at,lwd=1,col="#aabbff")
  if(length(D)<2) {
# Just one day's data, show time stamps
    H = unique(format(index(x)[hat],"%H:%M"))
    mtext(H, side=1,at=hat,line=0,cex=0.75)
    abline(v=hat,lwd=1,col="#aabbff")
  }
  lines(1:length(index(x)),x,col=col,lwd=lwd,lty=lty)
#  if(length(D)>1) {
#    for(j in seq(1,length(D)-1)){
#      i1 <- which((format(index(x),"%h %d")==D[j]))
#      i2 <- which((format(index(x),"%h %d")==D[j+1]))
#      y1 <- as.numeric(x[i1])
#      y2 <- as.numeric(x[i2])
#      lines(c(i1,i2), c(y1,y2), col=col, lwd=lwd, lty=lty)
#    }
#  }
# If a week or less, show hourly ticks
  if(length(D)<7)
    axis(side=1,at=hat,labels=FALSE,col="#555555",lwd.ticks=1)
}
