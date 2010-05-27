crapmod = function(x, title="", col=4, lwd=2, lty=1)
{
  p = as.numeric(x[,1])
  plot.new()
  par(mar=c(2.2,3,3,3))
  plot(p, axes=FALSE, type='l', col="#eeeeee",ylab="Close", main=title, xlab='')
  box()
  axis(side=2,pretty(p))
  axis(side=4,pretty(p))
  at = endpoints(x,on="days") + 1 
  at = at[-length(at)]
  hat = endpoints(x,on="hours") + 1 
  hat = hat[-length(at)]
  D = unique(format(index(x)[at],"%h %d"))
  axis(side=1,at=at,labels=D)
  abline(v=at,lwd=1,col="#aabbff")
  abline(h=pretty(p),lwd=1,col="#aabbff")
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
  axis(side=1,at=hat,labels=FALSE,col="#555555",lwd.ticks=1)
}
