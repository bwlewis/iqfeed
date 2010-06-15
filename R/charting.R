crapmod = function(x, title="", col=4, lwd=2, lty=1, xlim=NULL, ylim=NULL)
{
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
  abline(h=pretty(p),lwd=1,col="#aabbff")
  if(length(mat)>0) {
# Plot a whole year or more of data
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
  axis(side=1,at=at,labels=D)
  abline(v=at,lwd=1,col="#aabbff")
  if(length(D)<2) {
    H = unique(format(index(x)[hat],"%H:%M"))
    mtext(H, side=1,at=hat,line=0,cex=0.75)
    abline(v=hat,lwd=1,col="#aabbff")
  }
  for(j in D){
    i <- which((format(index(x),"%h %d")==j))
    y <- as.numeric(x[i])
    lines(i, y, col=col, lwd=lwd, lty=lty)
  }
  if(length(D)<7)
    axis(side=1,at=hat,labels=FALSE,col="#555555",lwd.ticks=1)
}
