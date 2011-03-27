# Warning, this may change!
.quoteFields <- c( 'Type', 'Symbol', 'Exchange ID', 'Last', 'Change', 'Percent Change', 'Total Volume', 'Incremental Volume', 'High', 'Low', 'Bid', 'Ask', 'Bid Size', 'Ask Size', 'Tick', 'Bid Tick', 'Range', 'Last Trade Time', 'Open Interest', 'Open', 'Close', 'Spread', 'Strike', 'Settle', 'Delay', 'Market Center', 'Restricted code', 'Net Asset Value', 'Average Maturity', 'Seven Day Yield', 'Last Trade Date', '(Reserved)', 'Extended Trading Last', 'Expiration Date', 'Regional Volume', 'Net Asset Value 2', 'Extended Trading Volume', 'Extended Trading Difference', 'Price-Earnings Ratio', 'Percent Off Average Volumn', 'Bid Change', 'Ask Change', 'Change From Open', 'Market Open', 'Volatility', 'Market Cap', 'Fraction Display Code', 'Decimal Precision', 'Days to Expiration', 'Previous Day Volumn', 'Regions', 'OpenRange1', 'CloseRange1', 'OpenRage2', 'CloseRange2', 'Number of Trades Today', 'Bid Time', 'Ask Time', 'VWAP', 'TickID', 'Financial Status Indicator', 'Settlement Date')

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
      names(retval) <- .quoteFields
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
