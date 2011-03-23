`iqQuote` <- 
  function(symbol, type="P", limit=10)
{
  cmd = paste("w",symbol,"\r\n",sep="")
  retval <- NULL
  tryCatch(
     {
      .iqConnect("level1")
      con <- .iqEnv$con["level1"][[1]]
      if(.iqBlock(con,write=TRUE)==FALSE) return(NULL)
      cat(cmd, file=con)
      retval <- .getQuote(type=type,sym=symbol,limit=limit)
     },
     error=function(e) {.iqClose("level1"); warning(e)})
  retval
}

.getQuote <- function(type, sym, limit)
{
  con <- .iqEnv$con["level1"][[1]]
  if(.iqBlock(con,write=FALSE)==FALSE) return(NULL)
  j <- 1
  x <- c()
  while(j<limit) {
    if(.iqBlock(con,write=FALSE)==FALSE) break
    dat <- tryCatch(readLines(con, n=1), error=function(e) warning(e))
    if(length(dat)>0) {
      y <- strsplit(dat,",")[[1]]
      if(length(y)>0)
        if(y[1]==type && y[2]==sym) {
          x <- y
          break
        }
    }
    j <- j + 1
  }
  .iqClose("level1")
  return(x) 
}
