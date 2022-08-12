# bifurcation-threshold

This repository contains code that was used to carry out the simulations and plot the figures in the following publication:

> Kauhanen, Henri (in prep) A bifurcation threshold for contact-induced language change. Ms., University of Konstanz.


## Instructions

To reproduce the simulations and plots, navigate to the `R` directory and type the following:

```r
source("requires.R")

orbs <- orbits()
pastim <- passage_time_sweep_afrikaans()
sims <- simulations()

plot_all()
```

The first line loads all required packages and scripts. The following R packages are needed: ggplot2, ggsci, gridExtra, plyr, reshape2, viridis. Please install these first if not already present on your system.

Computing the orbits takes less than a minute. The simulations take a rather longer while: FIXME minutes on a fairly powerful computer (Ryzen 3950X with 128GB of RAM). Code is not parallelized so that exact replication is possible (the `simulations()` routine implicitly sets the PRNG seed).

The resulting figures are outputted into the `plots` folder by default.


## Acknowledgements

This work was funded by the European Research Council as part of project STARFISH (851423).
