# This file contains various control functions.
`iqConf` <-
function(host='localhost', ports=list(5009,9100,9200))
{
# Stash state in the redis enivronment describing this connection:
  assign('host',host,envir=.iqEnv)
  assign('ports',ports,envir=.iqEnv)
  assign('timeout',7,envir=.iqEnv)
  invisible()
}

`iqTimeout` <-
function(timeout=5)
{
  assign('timeout',timeout,envir=.iqEnv)
  invisible()
}

`.iqConnect` <-
function(ports=.iqEnv$ports)
{
  con=vector("list",length(.iqEnv$ports))
  for(j in ports){
    k <- which(.iqEnv$ports == j)
    tryCatch(con[[k]] <- socketConnection(.iqEnv$host, j,open='a+b'),
              error=function(e) {invisible()},
              warning=function(e) {invisible()})
    k <- k + 1
  }
  assign('con',con,envir=.iqEnv)
}

`.iqClose` <- 
function()
{
  for(x in .iqEnv$con)
   tryCatch(
    {
     close(x)
    }, error=function(e) {invisible()})
   tryCatch(
    {
      remove(list='con',envir=.iqEnv)
    }, error=function(e) invisible(),
       warning=function(e) invisible()
   )
}
