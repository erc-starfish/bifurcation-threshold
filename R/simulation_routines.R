# Simulation routines
#
# (NB: These could be parallelized by applying parallel::mc(m|l)apply instead of plain
# (m|l)apply. However, predictable serial execution is favoured here for exact
# replicability of simulations.)
#
# Henri Kauhanen 2022



# Variational Learning of two grammars, with optional L2-difficulty
#
# ambient: probability of G1 in learner's environment
# d: amount of L2-difficulty (delta/gamma)
# gamma: base learning rate
# N: number of learning tokens (length of learning period)
# alpha1: advantage of G1
# alpha2: advantage of G2
# x0: starting value of probability of G1
#
VL <- function(ambient,
               d,
               gamma,
               N,
               alpha1,
               alpha2,
               x0 = runif(1)) {
  x <- rep(x0, length=N+1)
  pen1 <- alpha2*(1-ambient)
  pen2 <- alpha1*ambient

  for (n in 2:(N+1)) {
    chosen <- sample(1:2, size=1, prob=c(x[n-1], 1-x[n-1]))
    if (chosen == 1) {
      if (runif(1) < pen1) {
        x[n] <- x[n-1] - gamma*x[n-1] - d*gamma*x[n-1]
      } else {
        x[n] <- x[n-1] + gamma*(1 - x[n-1]) - d*gamma*x[n-1]
      }
    } else {
      if (runif(1) < pen2) {
        x[n] <- x[n-1] + gamma*(1 - x[n-1]) - d*gamma*x[n-1]
      } else {
        x[n] <- x[n-1] - gamma*x[n-1] - d*gamma*x[n-1]
      }
    }
  }

  x
}


# Learn one generation of VLs. For the meanings of the arguments, consult
# the function 'VL' above.
#
one_generation <- function(gamma,
                           d,
                           N,
                           alpha1,
                           alpha2,
                           ambient,
                           howmany) {
  df <- replicate(n=howmany, VL(gamma=gamma, d=d, N=N, alpha1=alpha1, alpha2=alpha2, ambient=ambient))
  df <- reshape2::melt(df)
  names(df) <- c("time", "learner", "p")
  df$gamma <- gamma
  df$d <- d
  df
}


# Learn an inter-generational chain of generations of VLs. In generation n (n > 1),
# each learner gets two "parents" from generation n-1. Parameters as in 'VL'.
# Additional parameters:
#
# ambient0: probability of G1 in the 0th generation
# howmany_per_generation: how many learners in each generation
# howmany_generations: how many generations to simulate
# sigma: proportion of L2 learners in each generation
#
intergenerational <- function(gamma,
                              d,
                              N,
                              alpha1,
                              alpha2,
                              ambient0,
                              howmany_per_generation,
                              howmany_generations,
                              sigma) {
  log <- vector("list", howmany_generations)

  dlog <- vector("list", howmany_generations)

  for (gen in 1:howmany_generations) {
    if (gen == 1) {
      # assign children to be L2 or L1 learners
      ds <- sample(c(d, 0), size=howmany_per_generation, replace=TRUE, prob=c(sigma, 1-sigma))

      log[[gen]] <- mapply(FUN=VL, ambient=rep(ambient0, howmany_per_generation), d=ds, MoreArgs=list(gamma=gamma, N=N, alpha1=alpha1, alpha2=alpha2))

      dlog[[gen]] <- ds
    } else {
      # randomly draw two parents for each child
      ambients <- rep(NA, length=howmany_per_generation)
      for (i in 1:length(ambients)) {
        parents <- sample(1:howmany_per_generation, size=2, replace=FALSE)
        ambients[i] <- mean(c(log[[gen-1]][, parents[1]], log[[gen-1]][, parents[2]]))
      }

      # assign children to be L2 or L1 learners
      ds <- sample(c(d, 0), size=howmany_per_generation, replace=TRUE, prob=c(sigma, 1-sigma))

      log[[gen]] <- mapply(FUN=VL, ambient=ambients, d=ds, MoreArgs=list(gamma=gamma, N=N, alpha1=alpha1, alpha2=alpha2))

      dlog[[gen]] <- ds
    }
  }

  for (l in 1:length(log)) {
    tmp <- cbind(reshape2::melt(log[[l]]), rep(l, N+1))
    tmp$d <- dlog[[l]]
    names(tmp) <- c("time", "learner", "p", "generation", "d")
    tmp <- tmp[tmp$time==N+1, ]
    log[[l]] <- tmp
  }

  df <- do.call(rbind, log)
  df$gamma <- gamma
  df
}
