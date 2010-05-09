`CEO` <- function(symbol,pc="pc",monthCodes="",near=1)
{
  .iqConnect()
  con <- .iqEnv$con[[2]]
# Check for a numeric month and convert to IQfeed month codes
  if(is.numeric(monthCodes)){
    monthCodes <- paste(LETTERS[c(monthCodes, 12 + monthCodes)],collapse="")
    near <- NULL
  }
  cmd <- paste("CEO",symbol, pc, monthCodes, near, "\r\n",sep=",")
  socketSelect(list(con), write=TRUE, timeout=.iqEnv$timeout)
  cat(cmd, file=con)
  .getChainData()
}

# Convert from IQFeed to OPRA OSI option symbol format
osi <- function(symbol)
{
  retval <- c()
  s <- sub("([A-Z]*)[0-9]*[A-X][0-9.]*$","\\1",symbol,extend=TRUE)
  m <- sub("[A-Z]*[0-9]*([A-X])[0-9.]*$","\\1",symbol,extend=TRUE)
  yd <- sub("[A-Z]*([0-9]*)[A-X][0-9.]*$","\\1",symbol,extend=TRUE)
  d <- as.integer(sub("..(.*)$","\\1",yd,extend=TRUE))
  y <- as.integer(sub("(..).*$","\\1",yd,extend=TRUE))
  p <- as.numeric(sub("([A-Z]*[0-9]*[A-X])([0-9.]*)$","\\2",symbol,extend=TRUE))
  s <- strtrim(sprintf("%s      ",s),6)
  for(j in 1:length(m)) {
    T <- "P"
    if(m[[j]] < "M") T <- "C"
    M <- which(LETTERS==m[[j]]) %% 12
    M <- M + (M==0)*12
    Y <- sprintf("%02.0f",y[[j]])
    M <- sprintf("%02.0f",M)
    D <- sprintf("%02.0f",d[[j]])
    p1 <- sprintf("%05.0f",p[[j]])
    p2 <- sub(".*[.]","",sprintf("%0.3f",p[[j]]))
    S <- s[[j]]
    retval <- c(retval, paste(S,Y,M,D,T,p1,p2,sep=""))
  }
  retval
}

# This one converts back to IQFeed symbology:
osi2iq <- function(symbol)
{
  s <- sub("^(......).*$","\\1",symbol,extended=TRUE)
  s <- sub("[[:blank:]].*","",s,extended=TRUE)
  y <- sub("^......(..).*$","\\1",symbol,extended=TRUE)
  m <- as.integer(sub("^........(..).*$","\\1",symbol,extended=TRUE))
  d <- sub("^..........(..).*$","\\1",symbol,extended=TRUE)
  t <- sub("^............(.).*$","\\1",symbol,extended=TRUE)
  p1 <- as.numeric(sub("^.............(.....).*$","\\1",symbol,extended=TRUE))
  p2 <- as.numeric(sub("^..................(...)$","\\1",symbol,extended=TRUE))
  p <- p1 + p2/1000
  m <- LETTERS[m + (t=="P")*12]
  paste(s,y,d,m,p,sep="")
}

.getChainData <- function() 
{
  con <- .iqEnv$con[[2]]
  socketSelect(list(.iqEnv$con[[2]]))
  dat <- tryCatch(readBin(con, 'raw', n=4096), error=function(e){})
  rlen <- 50
  j <- 1
  r <- vector('list',rlen)
  r[j] <- list(dat)
  if(grepl("!ENDMSG!", rawToChar(dat), useBytes=TRUE)) dat <- NULL
  while(length(dat)>0) {
    socketSelect(list(.iqEnv$con[[2]]),timeout=.iqEnv$timeout)
    dat <- tryCatch(readBin(con, 'raw', n=4096), error=function(e){})
    j <- j + 1
    if(j>rlen) {
      rlen <- 2*rlen
      length(r) <- rlen
    }
    r[j] <- list(dat)
    if(grepl("!ENDMSG!", rawToChar(dat), useBytes=TRUE)) dat <- NULL
  }
  .iqClose()
  r <- do.call(c,r)
  r <- rawToChar(r)
  r <- unlist(strsplit(r,"\r\n"))[1]
  r <- unlist(strsplit(r,","))
  r <- r[unlist(lapply(r,nchar))>2]
  r
}
