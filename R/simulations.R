# actual routines defined in 'simulation_routines.R'


simulations <- function() {
  # set RNG seed for reproducibility
  set.seed(20311344)

  # values of gamma (learning rate) to consider
  gammas <- c(0.1, 0.01, 0.001)

  # length of learning period
  N <- 100000

  # single generation simulations, for different gammas
  sg_L1 <- do.call(rbind, lapply(X=gammas, FUN=one_generation, d=0, N=N, alpha1=0.25, alpha2=0.2, ambient=0.5, howmany=5))
  sg_L2 <- do.call(rbind, lapply(X=gammas, FUN=one_generation, d=2, N=N, alpha1=0.25, alpha2=0.2, ambient=0.5, howmany=5))
  sg <- rbind(sg_L1, sg_L2)
  sg$ID <- paste0(sg$d, sg$learner, sep="_")

  # inter-generational simulations, for different gammas
  ig <- do.call(rbind, lapply(X=gammas, FUN=intergenerational, d=2, N=N, alpha1=0.25, alpha2=0.2, ambient0=0.99, howmany_per_generation=100, howmany_generations=20, sigma=0.5))

  # return
  list(sg=sg, ig=ig)
}

