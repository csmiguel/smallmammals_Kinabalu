###.............................................................................
# (c) Miguel Camacho Sánchez
# miguelcamachosanchez@gmail.com // miguelcamachosanchez.weebly.com
# https://scholar.google.co.uk/citations?user=1M02-S4AAAAJ&hl=en
# May 2019
###.............................................................................
#GOAL: plot beta diversity
#DESCRIPTION:
#PROJECT: https://github.com/csmiguel/smallmammals_Kinabalu
###.............................................................................

library(dplyr)

input1 <- "data/intermediate/betadiv_global.rds"
endemics <- readRDS(input1)

source("src/parameters/params.r")

#1. Plot beta diversity
permK <- reshape2::melt(endemics$permK) %>% dplyr::select(-1)
permT <- reshape2::melt(endemics$permT) %>% dplyr::select(-1)

plot_beta_div <- function(dataset, boff, colmt, permobject){
  stripchart(value~Var2, vertical = TRUE, data = permobject,
           method = "jitter", add = TRUE, pch = 20,
           col = adjustcolor("lightblue", 0.02), at = seq(1, 3, 1) + boff)
  for (i in 1:3){
    segments(x0 = i + boff, y0 = dataset[4, i],
             x1 = i + boff, y1 = dataset[6, i], col = "grey", lwd = 5)
    segments(x0 = i - 0.15 + boff, y0 = dataset[1, i],
             x1 = i + 0.15 + boff, y1 = dataset[1, i], col = colmt, lwd = 2)
    segments(x0 = i - 0.15 + boff, y0 = dataset[2, i],
             x1 = i + 0.15 + boff, y1 = dataset[2, i],
             col = "red", lwd = 2, lty = 2)
  }
  }
pdf("output/Figure5_beta_diversity.pdf", width = 5 * 1.2, height = 5)
  boxplot(value~Var2, data = permK, lwd = 2,
    ylab = "Index value", border = "white", las = 1)
  plot_beta_div(endemics$summary$Kinabalu, - 0.2, cols[1], permK)
  plot_beta_div(endemics$summary$Tambuyukon, 0.2, cols[2], permT)
dev.off()
