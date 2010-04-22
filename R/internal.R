.iqEnv <- new.env()

# .iqError may be called by any function when a serious error occurs.
# It will print an indicated error message, attempt to reset the current
# IQFeed server connection, and signal the error.
.iqError <- function(msg)
{
  stop(msg)
}
