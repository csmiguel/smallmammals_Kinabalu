###.............................................................................
# (c) Miguel Camacho Sánchez
# miguelcamachosanchez@gmail.com // miguelcamachosanchez.weebly.com
# https://scholar.google.co.uk/citations?user=1M02-S4AAAAJ&hl=en
# May 2019
###.............................................................................
#GOAL: plot alpha diversity
#DESCRIPTION:
#PROJECT: https://github.com/csmiguel/smallmammals_Kinabalu
###.............................................................................

library(dplyr)

input1 <- "data/intermediate/models_alpha_diversity.rds"
div <- readRDS(input1)
input2 <- "data/intermediate/rangemodel.rds"
rangemodel <- readRDS(input2)
source("src/functions/plot_div.r")
source("src/parameters/params.r")

#2. Plot alpha diversity
  #plotting objects
y_lim <- c(0, max(rangemodel$Tambuyukon$q975, div$Richness))
index <- c("Richness", "Shannon", "Simpson", "Evenness")
labs <- c("Species richness S", "Shannon H'", "Simpson D", "Evenness J'")
col_site <- cols[div$Location]
draw_xaxis <- c("n", "n", "s", "s")
panel <- c("(A)", "(B)", "(C)", "(D)")
psize <- 4.5

  #start plot
pdf("output/Figure3_alpha_diversity.pdf", height = psize, width = psize * 1.66)
par(mfrow = c(2, 2), oma = rep(1, 4), mar = c(1, 2, 1, 1), las = 1)
for (i in seq_along(index)){
  indexi <- index[i]
  if (indexi == "Richness"){
    plot(div$Elevation, div[indexi][, 1], col = col_site,
      pch = as.numeric(div$Location), cex = 1,
      xlab = "",
      ylab = "",
      #ylab = labs[i],
      ylim = y_lim, xaxt = draw_xaxis[i])
    #results from rangemodelR
    plot_rangemodelr(rangemodel, "Kinabalu", cols[1])
    plot_rangemodelr(rangemodel, "Tambuyukon", cols[2])
    plot_pred("Tambuyukon", m = "m1")
    plot_pred("Kinabalu", m = "m1")
  } else if (indexi != "Richness"){
  plot(div$Elevation, div[indexi][, 1], col = col_site,
    pch = as.numeric(div$Location),
    #ylab = labs[i],
    ylab = "",
    xlab = "", xaxt = draw_xaxis[i])
  }
  lowess_line("Kinabalu", indexi)
  lowess_line("Tambuyukon", indexi)
  legend("topright", paste(panel[i], labs[i]), bty = "n", cex = .9)
}
dev.off()
