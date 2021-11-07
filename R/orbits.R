# Iterate the system until the difference between state at n and n+1
# is less than 'lim' in both dimensions.
limit_point <- function(p0,
                        q0,
                        alpha1,
                        alpha2,
                        sigma,
                        d,
                        lim = 10^-9) {
  p <- p0
  q <- q0

  oldp <- 10
  oldq <- 10

  iterations <- 0

  while (abs(oldp - p) > lim & abs(oldq - q) > lim) {
    oldp <- p
    oldq <- q
    iterations <- iterations + 1
    newstate <- iterator(x=c(p, q), alpha1=alpha1, alpha2=alpha2, sigma=sigma, d=d)
    p <- newstate[1]
    q <- newstate[2]
  }

  data.frame(p=p, q=q, iter=iterations)
}


# "Orbit analysis": find the limit point for ranges of parameter values.
orbits <- function(a = c(1.5, 2.0, 3.0),
                   D = seq(from=0.1, to=10, length.out=50),
                   sigma = seq(from=0.1, to=0.9, length.out=50),
                   lim = 10^-9) {
  df <- expand.grid(a=a, D=D, sigma=sigma)
  df$p <- NA
  df$q <- NA

  for (i in 1:nrow(df)) {
    alpha1 <- 0.5
    alpha2 <- alpha1/df[i,]$a
    d <- alpha2*df[i,]$D

    result <- limit_point(p0=runif(1), q0=runif(1), alpha1=alpha1, alpha2=alpha2, sigma=df[i,]$sigma, d=d, lim=lim)

    df[i,]$p <- result$p
    df[i,]$q <- result$q
  }

  df
}


