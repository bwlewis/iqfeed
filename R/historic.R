`HIT` <- 
  function(symbol, interval=3600,start,end=format(Sys.time(),format="%Y-%m-%d"),beginFilterTime="",endFilterTime="", tz="America/New_York")
{
  start <- format(as.POSIXct(start,tz=tz),"%Y%m%d %H%M%S")
  end <- format(as.POSIXct(end,tz=tz),"%Y%m%d %H%M%S")
  cmd <- paste("HIT",symbol, interval, start, end, "", beginFilterTime,
                endFilterTime, 0, "\r\n",sep=",")
  retval <- NULL
  tryCatch(
     {
      .iqConnect("historic")
      con <- .iqEnv$con["historic"][[1]]
      if(.iqBlock(con,write=TRUE)==FALSE) return(NULL)
      cat(cmd, file=con)
      retval <- .getHistoricData(tz=tz)
     },
     error=function(e) {.iqClose("historic"); warning(e)})
  retval
}

`HTT` <- 
  function(symbol,start,end=format(Sys.time(),format="%Y-%m-%d"),beginFilterTime="",endFilterTime="", tz="America/New_York")
{
  start <- format(as.POSIXct(start,tz=tz),"%Y%m%d %H%M%S")
  end <- format(as.POSIXct(end,tz=tz),"%Y%m%d %H%M%S")
  cmd <- paste("HTT",symbol, start, end, "", beginFilterTime,
                endFilterTime, 0, "\r\n",sep=",")
  retval <- NULL
  tryCatch(
     {
      .iqConnect("historic")
      con <- .iqEnv$con["historic"][[1]]
      if(.iqBlock(con,write=TRUE)==FALSE) return(NULL)
      cat(cmd, file=con)
      retval <- .getTickData(tz)
     },
     error=function(e) {.iqClose("historic"); warning(e)})
  retval
}

`HDX` <- 
  function(symbol,days,tz="")
{
  cmd <- paste("HDX",symbol,days=0,"\r\n",sep=",")
  retval <- NULL
  tryCatch(
     {
      .iqConnect("historic")
      con <- .iqEnv$con["historic"][[1]]
      if(.iqBlock(con,write=TRUE)==FALSE) return(NULL)
      cat(cmd, file=con)
      retval <- .getHistoricData(tz=tz,volcol=6)
     },
     error=function(e) {.iqClose("historic"); warning(e)})
  retval
}

.getHistoricData <- function(tz,volcol=7)
{
  con <- .iqEnv$con["historic"][[1]]
  if(.iqBlock(con,write=FALSE)==FALSE) return(NULL)
  dat <- tryCatch(readBin(con, 'raw', n=65536), error=function(e) warning(e))
  rlen <- 50
  j <- 1
  r <- vector('list',rlen)
  r[j] <- list(dat)
  if(grepl("!ENDMSG!", rawToChar(dat), useBytes=TRUE)) dat <- NULL
  while(length(dat)>0) {
    if(.iqBlock(con,write=FALSE)==FALSE) return(NULL)
    dat <- tryCatch(readBin(con, 'raw', n=65536), error=function(e) warning(e))
    j <- j + 1
    if(j>rlen) {
      rlen <- 2*rlen
      length(r) <- rlen
    }
    r[j] <- list(dat)
    if(grepl("!ENDMSG!", rawToChar(dat), useBytes=TRUE)) dat <- NULL
  }
  .iqClose("historic")
  z <- NULL
  tryCatch({
    r <- do.call(c,r)
    r <- rawToChar(r)
    r <- unlist(strsplit(r,"\r\n"))
    r <- r[r!="!ENDMSG!,"]
    y <- strsplit(r,',')
    z <- t(as.data.frame(y,stringsAsFactors=FALSE))
    rownames(z) <- NULL
    z <- as.data.frame(z,stringsAsFactors=FALSE)
    z[,1] <- as.POSIXct(z[,1],tz=tz)
    z[,2] <- as.numeric(z[,2])
    z[,3] <- as.numeric(z[,3])
    z[,4] <- as.numeric(z[,4])
    z[,5] <- as.numeric(z[,5])
    z[,6] <- as.numeric(z[,volcol])
    z <- z[,1:6]
    if(ncol(z)==6) {
      colnames(z)<-c("Date","High","Low","Open","Close","Volume")
      z <- xts(z[,2:6],order.by=z[,1])
    }
  },error=function(e) warning(e))
  z
}

.getTickData <- function(tz)
{
  con <- .iqEnv$con["historic"][[1]]
  if(.iqBlock(con,write=FALSE)==FALSE) return(NULL)
  dat <- tryCatch(readBin(con, 'raw', n=65536), error=function(e) warning(e))
  rlen <- 50
  j <- 1
  r <- vector('list',rlen)
  r[j] <- list(dat)
  if(grepl("!ENDMSG!", rawToChar(dat), useBytes=TRUE)) dat <- NULL
  while(length(dat)>0) {
    if(.iqBlock(con,write=FALSE)==FALSE) return(NULL)
    dat <- tryCatch(readBin(con, 'raw', n=65536), error=function(e) warning(e))
    j <- j + 1
    if(j>rlen) {
      rlen <- 2*rlen
      length(r) <- rlen
    }
    r[j] <- list(dat)
    if(grepl("!ENDMSG!", rawToChar(dat), useBytes=TRUE)) dat <- NULL
  }
  .iqClose("historic")
  z <- NULL
  tryCatch({
    r <- do.call(c,r)
    r <- rawToChar(r)
    r <- unlist(strsplit(r,"\r\n"))
    r <- r[r!="!ENDMSG!,"]
    y <- strsplit(r,',')
    z <- t(as.data.frame(y,stringsAsFactors=FALSE))
    rownames(z) <- NULL
    z <- as.data.frame(z,stringsAsFactors=FALSE)
    z[,1] <- as.POSIXct(z[,1],tz=tz)
    z[,2] <- as.numeric(z[,2])
    z[,3] <- as.numeric(z[,3])
    z[,4] <- as.numeric(z[,4])
    z[,5] <- as.numeric(z[,5])
    z[,6] <- as.numeric(z[,6])
    z[,7] <- as.numeric(z[,7])
    z[,8] <- as.numeric(z[,8])
    z[,9] <- as.numeric(z[,9])
    z <- z[,1:9]
    if(ncol(z)==9) {
      colnames(z)<-c("Date","Last","Size","Volume","Bid","Ask","TickID","Bid Size", "Ask Size")
      z <- xts(z[,2:9],order.by=z[,1])
    }
  },error=function(e) warning(e))
  z
}
