# Returns the population state in generation n+1, given state in n.
iterator <- function(x,
                     alpha1,
                     alpha2,
                     sigma,
                     d) {
  pi1 <- (1-sigma)*alpha2*(1-x[1]) + sigma*alpha2*(1-x[2])
  pi2 <- (1-sigma)*alpha1*x[1] + sigma*alpha1*x[2]
  
  c(pi2/(pi1 + pi2), pi2/(pi1 + pi2 + d))
}


# Returns the time derivative of the population state at x (including the
# denominator, which in the mathematical analysis was dropped for simplicity).
derivative <- function(x,
                       alpha1,
                       alpha2,
                       sigma,
                       d) {
  pi1 <- (1-sigma)*alpha2*(1-x[1]) + sigma*alpha2*(1-x[2])
  pi2 <- (1-sigma)*alpha1*x[1] + sigma*alpha1*x[2]

  c(pi2/(pi1 + pi2) - x[1], pi2/(pi1 + pi2 + d) - x[2])
}
