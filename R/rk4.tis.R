#' Runge-Kutta for Time-Invariant Systems
#'
#' Numerical solution of a time-invariant (autonomous) ordinary differential equation using the 4th-order Runge-Kutta method.
#'
#' @param x0 Initial state; may be a vector for a many-dimensional system
#' @param h Step size
#' @param steps How many steps to take
#' @param FUN Function that returns the time derivative of the system's state
#' @param timetravel If \code{TRUE}, the backwards solution is computed instead of the forward trajectory
#' @param ... Further parameters passed to \code{FUN}
#' @return A data frame with columns \code{x1} to \code{xn}, where \code{n} is the system's dimension, \code{step} which indicates the current step, and \code{t} which indicates current time. The data frame has \code{steps} + 1 rows, the first row giving the initial state at time 0.
#'
#' @export
rk4.tis <- function(x0,
                    h,
                    steps,
                    FUN,
                    timetravel = FALSE,
                    ...) {
  take_one_step <- function(x, h, ...) {
    k1 <- h*FUN(x, ...)
    k2 <- h*FUN(x + k1/2, ...)
    k3 <- h*FUN(x + k2/2, ...)
    k4 <- h*FUN(x + k3, ...)
    x + (k1 + 2*k2 + 2*k3 + k4)/6
  }

  if (timetravel) {
    oldFUN <- FUN
    FUN <- function(x, ...) {
      -oldFUN(x=x, ...)
    }
  }

  out <- as.data.frame(matrix(NA, nrow=steps+1, ncol=length(x0)))

  xnow <- x0
  for (i in 1:(steps+1)) {
    out[i, ] <- xnow
    xnow <- take_one_step(x=xnow, h=h, ...)
  }

  out$step <- 0:steps
  out$t <- out$step*h
  names(out)[1:length(x0)] <- paste0("x", 1:length(x0))

  out
}
