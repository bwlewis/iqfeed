.iqEnv <- new.env()

# .iqError may be called by any function when a serious error occurs.
# It will print an indicated error message, attempt to reset the current
# IQFeed server connection, and signal the error.
.iqError <- function(msg)
{
  stop(msg)
}

.iqBlock <- function(con, write=TRUE)
{
  ret <- TRUE
  tryCatch(
    socketSelect(list(con), write=write, timeout=.iqEnv$timeout),
    error=function(e) {print(e);ret <<- FALSE}
  )
  if(ret == FALSE)
    tryCatch(close(con),error=function(e) invisible())
  ret
}

