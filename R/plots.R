# Plot all figures for the paper. Output goes in the 'plots' folder.
plot_all <- function(folder = "../plots") {
  # orbits
  cairo_pdf(paste0(folder, "/Figure1.pdf"), height=5.0, width=6.5)
  print(plot_orbits(orbs))
  dev.off()

  # simulations
  cairo_pdf(paste0(folder, "/Figure2.pdf"), height=8, width=7)
  print(plot_simulations(sims))
  dev.off()

  # passage times
  cairo_pdf(paste0(folder, "/Figure3.pdf"), height=3.2, width=6.5)
  print(plot_passage_times(pastim))
  dev.off()
}


# Plot orbit diagram.
plot_orbits <- function(data = orbs) {
  a <- c(1.5, 2.0, 3.0)
  crit <- expand.grid(a=a, D=seq(from=0.0, to=10.0, length.out=1000))
  crit$sigma <- ((crit$a - 1)*(crit$D + 1))/(crit$a*crit$D)

  orb <- melt(data, id.vars=c("a", "D", "sigma"))
  orb$variable <- factor(orb$variable, labels=c("p (L1 population)", "q (L2 population)"))
  g <- ggplot(orb, aes(x=D, y=sigma, fill=value)) + geom_raster() + scale_fill_viridis(option="turbo", name="", limits=c(0,1)) 
  g <- g + facet_grid(variable~a, labeller = label_bquote(.(variable), alpha==.(a)))
  g <- g + theme_bw()
  g <- g + xlab(expression("Normalized relative L2-difficulty"~italic(D))) + ylab(expression("Proportion of L2 speakers"~sigma))
  g <- g + scale_x_continuous(expand=c(0,0), breaks=c(1, 3, 5, 7, 9), labels=c("1.0", "3.0", "5.0", "7.0", "9.0"), limits=c(min(orb$D), max(orb$D)))
  g <- g + scale_y_continuous(expand=c(0,0), breaks=c(0.1, 0.3, 0.5, 0.7, 0.9), limits=c(min(orb$sigma), max(orb$sigma)))
  g <- g + theme(panel.spacing = unit(1.0, "lines"), strip.placement = "outside", strip.background=element_rect(fill="white", color="white"), legend.position="top", axis.text=element_text(color="black"), strip.text=element_text(size=10))

  g <- g + geom_path(data=crit, inherit.aes=FALSE, aes(x=D, y=sigma), color="white", lwd=0.8, lty=2)

  g
}


# Plot passage time analysis for Afrikaans case study.
plot_passage_times <- function(data = pastim) {
  g <- ggplot(data, aes(x=d, y=passtime, color=factor(sigma))) + geom_path() + geom_point() + facet_wrap(.~q0, ncol=3, labeller = label_bquote(q[0]==.(q0))) + theme_bw() + ylab("Passage time (generations)") + xlab(expression("Relative L2-difficulty"~italic(d))) + scale_color_locuszoom(name="Proportion of L2 speakers: ", labels=expression(sigma==0.2, sigma==0.6))
  g <- g + theme(panel.spacing = unit(1.0, "lines"), strip.placement = "outside", strip.background=element_rect(fill="white", color="white"), legend.position="top", legend.text=element_text(size=11), strip.text=element_text(size=10), axis.text=element_text(color="black"), panel.grid.minor=element_blank())
  g <- g + scale_x_log10() + annotation_logticks(sides="b")
  g
}


# Plot simulation outcomes.
plot_simulations <- function(data = sims) {
  sg <- data$sg
  ig <- data$ig

  # theme
  mytheme <- theme_bw() + theme(axis.text=element_text(color="black"), strip.background=element_blank(), strip.text=element_text(size=12), legend.background=element_rect(colour = 'grey60', fill = 'white', size=0.3, linetype='solid'))


  # labeller function
  label_parseall <- function(variable, value) {
    plyr::llply(value, function(x) parse(text = paste(variable, x, sep = "==")))
  }


  # inter-generational plot
  ig$Population <- factor(ig$d, labels=c("L1", "L2"))
  ig <- ig[ig$generation <= 15, ]
  plot_ig <- ggplot(ig, aes(x=factor(generation), fill=Population, color=Population, y=p)) + geom_boxplot(color="grey60", alpha=0.6, outlier.size=0.8, outlier.shape=NA) + facet_wrap(.~gamma, ncol=1, labeller=label_parseall) + ggtitle("B") + mytheme + xlab("Generation") + ylab(expression("Probability of"~italic(G)[1]*" ("*italic(p)*", "*italic(q)*")"))

  prediction <- iterate(x0=c(0.99, 0.99), iter=20, alpha1=0.25, alpha2=0.2, sigma=0.5, d=2)
  prediction <- melt(prediction, id.vars="t")
  names(prediction) <- c("generation", "Population", "p")
  prediction$Population <- factor(prediction$Population, labels=c("L1", "L2"))
  prediction <- prediction[prediction$generation > 0, ]
  prediction <- prediction[prediction$generation <= 15, ]

  plot_ig <- plot_ig + geom_point(data=prediction, size=2.0, alpha=0.8, aes(x=generation, y=p, color=Population)) + geom_path(data=prediction, size=0.7, alpha=0.8, aes(x=generation, y=p, color=Population, group=Population)) + scale_color_nejm(guide = guide_legend(title.position = "top")) + theme(legend.position=c(0.78, 0.947), panel.grid.minor=element_blank(), legend.direction="horizontal", axis.text.x=element_text(angle=60, hjust=1), panel.grid.major.x=element_blank())


  # one generation plot
  sg <- sg[sg$time <= 10001, ]
  sg$Population <- factor(sg$d, labels=c("L1", "L2"))
  plot_sg <- ggplot(sg[sg$time %% 100 == 1, ], aes(x=time, group=ID, color=Population, y=p)) + geom_path(alpha=0.7) + facet_wrap(.~gamma, ncol=1, labeller=label_parseall) + mytheme + scale_color_nejm(guide = guide_legend(title.position = "top")) + xlab("Learning iteration") + ylab(expression("Probability of"~italic(G)[1]*" ("*italic(p)*", "*italic(q)*")")) + ggtitle("A") + theme(legend.position=c(0.78, 0.947), panel.grid.minor=element_blank(), legend.direction="horizontal")


  # combine them and out
  grid.arrange(plot_sg, plot_ig, ncol=2)
}
