# bifurcation-threshold

This repository contains code that was used to plot the figures in the following publication:

> Kauhanen, Henri (in prep) A bifurcation threshold for contact-induced language change. Ms., University of Konstanz.


## Instructions

To reproduce the plots, navigate to the `R` directory and type the following:

```r
source("requires.R")

orbs <- orbits()
pastim <- passage_time_sweep_afrikaans()
sols <- solutions()

plot_all()
```

The first line loads all required packages and scripts. The following R packages are needed: ggplot2, ggsci, reshape2, viridis. Please install these first if not already present on your system.

Computing the orbits and solutions may take a couple of minutes. The resulting figures will be outputted into the `plots` folder by default.


## Acknowledgements

This work has been supported by the European Research Council as part of project STARFISH (851423).
