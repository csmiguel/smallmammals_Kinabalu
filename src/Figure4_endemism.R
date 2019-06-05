###.............................................................................
# (c) Miguel Camacho Sánchez
# miguelcamachosanchez@gmail.com // miguelcamachosanchez.weebly.com
# https://scholar.google.co.uk/citations?user=1M02-S4AAAAJ&hl=en
# May 2019
###.............................................................................
#DESCRIPTION: Plot endemism across elevation
#PROJECT: https://github.com/csmiguel/smallmammals_Kinabalu
###.............................................................................

source("src/parameters/params.r")
source("src/functions/plot_bootstrap_endemics.r")
load("data/intermediate/plot_endemics.Rdata")

#plot
pdf("output/Figure4_endemism.pdf", width = 4 * 2.3, height = 4)
par(mfrow = c(1, 2), oma = c(1, 0, 1, 0), mar = c(4, 5, 1, 1), cex.axis = 0.8)
# 3.1 Endemic species elevation (point 2)
  plot_bootstrap_endemics(n_end, site_perm, "Proportion of Bornean endemics",
  mt, cols)
  text(520, 0.96, "A")
# 3.2 Endemic catches elevation (point 2)
  plot_bootstrap_endemics(end_catches, perm_catches,
    "Proportion of catches from Bornean endemics", mt, cols)
  text(520, 0.96, "B")
dev.off()
