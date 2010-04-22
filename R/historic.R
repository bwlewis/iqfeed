`HIT` <- 
  function(symbol, interval=3600,start,end,beginFilterTime="",endFilterTime="", tz="EDT")
{
  .iqConnect()
  con <- .iqEnv$con[[2]]
  start <- format(as.POSIXct(start,tz=tz),"%Y%m%d %H%M%S")
  end <- format(as.POSIXct(end,tz=tz),"%Y%m%d %H%M%S")
  cmd <- paste("HIT",symbol, interval, start, end, "", beginFilterTime,
                endFilterTime, 0, "\r\n",sep=",")
#  socketSelect(list(con), write=TRUE)
  cat(cmd, file=con)
  .getHistoricData()
}

.getHistoricData <- function() 
{
  con <- .iqEnv$con[[2]]
  socketSelect(list(.iqEnv$con[[2]]))
  dat <- tryCatch(readBin(con, 'raw', n=4096), error=function(e){})
  rlen <- 50
  j <- 1
  r <- vector('list',rlen)
  r[j] <- list(dat)
  while(length(dat)>0) {
    socketSelect(list(.iqEnv$con[[2]]),timeout=.iqEnv$timeout)
    dat <- tryCatch(readBin(con, 'raw', n=4096), error=function(e){})
    j <- j + 1
    if(j>rlen) {
      rlen <- 2*rlen
      length(r) <- rlen
    }
    r[j] <- list(dat)
  }
  .iqClose()
  r <- do.call(c,r)
  r <- rawToChar(r)
  r <- unlist(strsplit(r,"\r\n"))
  r <- r[r!="!ENDMSG!,"]
  y <- strsplit(r,',')
  z <- t(as.data.frame(y,stringsAsFactors=FALSE))
  rownames(z) <- NULL
  z <- as.data.frame(z,stringsAsFactors=FALSE)
  z[,1] <- as.POSIXct(z[,1])
  z[,2] <- as.numeric(z[,2])
  z[,3] <- as.numeric(z[,3])
  z[,4] <- as.numeric(z[,4])
  z[,5] <- as.numeric(z[,5])
  z[,6] <- as.numeric(z[,7])
  z <- z[,1:6]
  colnames(z)<-c("Date","High","Low","Open","Close","Volume")
  z <- xts(z[,2:6],order.by=z[,1])
  z
}
