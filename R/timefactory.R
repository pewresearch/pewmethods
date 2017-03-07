#' Create running timer functions
#'
#' This is a factory function for creating running timers. A function
#' created using \code{timefactory()} will return the number of seconds
#' since it was first created. It is intended as a simple alternative to
#' wrapping code in multiple calls to \code{proc.time()}.
#'
#' @return A function that will return the number of seconds since it
#' was first created.
#'
#' @examples
#' # Create an initial timer
#' timer1 = timefactory()
#' Sys.sleep(3)
#' timer1()
#'
#' # Create a second timer that is independent of the first.
#' timer2 = timefactory()
#' Sys.sleep(2)
#'
#' timer1()
#' timer2()
#'
#' @export
timefactory <- function(){
  t1 <- proc.time()

  function() {
    t1 <<- t1
    t2 <- proc.time() - t1
    return(t2[[3]])
  }

}
