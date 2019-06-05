###.............................................................................
# (c) Miguel Camacho Sánchez
# miguelcamachosanchez@gmail.com // miguelcamachosanchez.weebly.com
# https://scholar.google.co.uk/citations?user=1M02-S4AAAAJ&hl=en
# May 2019
###.............................................................................
#GOAL: plot neighbor joining tree
#DESCRIPTION:
#PROJECT: https://github.com/csmiguel/smallmammals_Kinabalu
###.............................................................................
library("magrittr")
library("ape")
tk <- readRDS("data/intermediate/nj_tree.rds")
source("src/parameters/params.r")
# add "m" to labels
tk$tip.label %<>% paste("m")
# create color vector for plotting
col_tip <- tk$tip.label %>%
  gsub(pattern = "_.*$", replacement = "") %>%
  {cols[as.factor(.)]}
#  plot
pdf("output/FigureS3_clustering.pdf", height = 5.5 * 1.3, width = 5.5)
plot(tk, tip.color = col_tip, font = 1, align.tip.label = T)
ape::axisPhylo()
dev.off()
