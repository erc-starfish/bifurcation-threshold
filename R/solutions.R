# Obtain solutions of the continuous-time system using Runge-Kutta.
solutions <- function(alpha1 = 0.3,
                      alpha2 = 0.2,
                      sigmas = seq(from=0.1, to=0.9, by=0.1),
                      d = 2,
                      x0_1 = c(1.0, 0.5),
                      x0_2 = c(0.5, 1.0),
                      h = 0.01,
                      iter = 10000) {
  out <- vector("list", length(sigmas))
  for (i in 1:length(sigmas)) {
    here <- rk4.tis(x0=x0_1, h=h, steps=iter, FUN=derivative, alpha1=alpha1, alpha2=alpha2, sigma=sigmas[i], d=d)
    here$sigma <- sigmas[i]
    out[[i]] <- here
  }
  out <- do.call(rbind, out)
  out$traj <- 1

  out2 <- vector("list", length(sigmas))
  for (i in 1:length(sigmas)) {
    here <- rk4.tis(x0=x0_2, h=h, steps=iter, FUN=derivative, alpha1=alpha1, alpha2=alpha2, sigma=sigmas[i], d=d)
    here$sigma <- sigmas[i]
    out2[[i]] <- here
  }
  out2 <- do.call(rbind, out2)
  out2$traj <- 2

  rbind(out, out2)
}
