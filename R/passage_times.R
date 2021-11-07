# Compute passage time (in generations) to reach a desired convergence to
# attractor.
passage_time <- function(p0,
                         q0,
                         alpha1,
                         alpha2,
                         sigma,
                         d,
                         lim = 10^-2) {
  p <- p0
  q <- q0

  iterations <- 0

  while (p > lim & q > lim) {
    iterations <- iterations + 1
    newstate <- iterator(x=c(p, q), alpha1=alpha1, alpha2=alpha2, sigma=sigma, d=d)
    p <- newstate[1]
    q <- newstate[2]
  }

  iterations
}


# Compute passage times in the Afrikaans case study, sweeping over
# relevant parameter value ranges.
passage_time_sweep_afrikaans <- function(p0 = 1,
                                         q0 = c(0.1, 0.5, 0.9),
                                         alpha1 = 5/6,
                                         alpha2 = 5/6,
                                         sigma = c(0.2, 0.6),
                                         d = seq(from=1, to=50, length.out=20),
                                         lim = 10^-2) {
  out <- expand.grid(q0=q0, sigma=sigma, d=d)
  out$passtime <- NA

  for (i in 1:nrow(out)) {
    out[i,]$passtime <- passage_time(p0=p0, q0=out[i,]$q0, alpha1=alpha1, alpha2=alpha2, sigma=out[i,]$sigma, d=out[i,]$d, lim=lim)
  }

  out
}
