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


# Iterate the system for 'iter' generations.
iterate <- function(x0,
                    iter,
                    alpha1,
                    alpha2,
                    sigma,
                    d,
                    scaler = 1.0) {
  out <- data.frame(t=0:iter, p=NA, q=NA)

  out[1,]$p <- x0[1]
  out[1,]$q <- x0[2]

  for (t in 2:(iter+1)) {
    result <- iterator(x=c(out[t-1,]$p, out[t-1,]$q), alpha1=alpha1, alpha2=alpha2, sigma=sigma, d=d)
    out[t,]$p <- scaler*result[1]
    out[t,]$q <- scaler*result[2]
  }

  out
}


