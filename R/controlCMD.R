# This file contains various control functions.
`iqConf` <-
function(host='localhost', ports=list(level1=5009,historic=9100,level2=9200))
{
# Stash state in the redis enivronment describing this connection:
  con <- vector("list",length=length(ports))
  names(con) <- names(ports)
  assign('host',host,envir=.iqEnv)
  assign('ports',ports,envir=.iqEnv)
  assign('con',con,envir=.iqEnv)
  assign('timeout',7,envir=.iqEnv)
  invisible()
}

`iqTimeout` <-
function(timeout=7)
{
  assign('timeout',timeout,envir=.iqEnv)
  invisible()
}

# Connect to a single port
`.iqConnect` <-
function(port)
{
  port <- port[[1]]
  .iqEnv$con[port][[1]] <- socketConnection(.iqEnv$host, .iqEnv$ports[port][[1]],open='a+b')
}

# Close a port
`.iqClose` <- 
function(port)
{
  port <- port[[1]]
  tryCatch(
     close(.iqEnv$con[port][[1]]),
     error=function(e) invisible()
  )
}
