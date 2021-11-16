# Plot all figures for the paper. Output goes in the 'plots' folder.
plot_all <- function(folder = "../plots") {
  # phase space plot
  pdf(paste0(folder, "/Figure1.pdf"), height=6.5, width=6.5)
  print(plot_phasespace(sols))
  dev.off()

  # orbits
  pdf(paste0(folder, "/Figure2.pdf"), height=5.0, width=6.5)
  print(plot_orbits(orbs))
  dev.off()

  # passage times
  pdf(paste0(folder, "/Figure3.pdf"), height=3.2, width=6.5)
  print(plot_passage_times(pastim))
  dev.off()
}


# Plot phase space plot.
plot_phasespace <- function(sols = sols,
                            sigmas = seq(from=0.1, to=0.9, by=0.1),
                            alpha1 = 0.3,
                            alpha2 = 0.2,
                            d = 2) {
  sfmajor <- NULL

  for (sigma in sigmas) {
  sf <- slope_field(FUN=derivative, alpha1=alpha1, alpha2=alpha2, sigma=sigma, d=2, len=0.05, resolution=10, normalize=TRUE)
  sf$sigma <- sigma
  sfmajor <- rbind(sfmajor, sf)
  }
  
  g <- ggplot(sfmajor, aes(x=x1, y=x2, xend=x1end, yend=x2end)) + geom_segment(arrow=arrow(angle=30, length=unit(0.1, "cm")), alpha=0.3, lwd=0.5) + theme_bw() + facet_wrap(.~sigma, labeller=label_bquote(sigma==.(sigma)))
  g <- g + geom_path(data=sols, inherit.aes=FALSE, aes(x=x1, y=x2, group=traj, color=factor(traj)), lwd=1.0, alpha=0.7) + scale_color_locuszoom()

  endmajor <- sols[sols$t == max(sols$t), ]
  endmajor <- endmajor[endmajor$traj==1, ]

  g <- g + geom_point(data=endmajor, inherit.aes=FALSE, aes(x=x1, y=x2), size=2.0, color="black", alpha=1.0)

  g <- g + theme(legend.position="none") + xlab(expression("Probability of"~italic(G)[1]~"in L1 population ("*italic(p)*")")) + ylab(expression("Probability of"~italic(G)[1]~"in L2 population ("*italic(q)*")"))

  g <- g + theme(panel.spacing = unit(1.0, "lines"), strip.placement = "outside", strip.background=element_rect(fill="white", color="white"), strip.text=element_text(size=10), axis.text=element_text(color="black"), aspect.ratio=1, panel.grid.minor=element_blank(), panel.grid.major=element_blank())

  g
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
