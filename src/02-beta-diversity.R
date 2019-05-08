###.............................................................................
# (c) Miguel Camacho Sánchez
# miguelcamachosanchez@gmail.com // miguelcamachosanchez.weebly.com
# https://scholar.google.co.uk/citations?user=1M02-S4AAAAJ&hl=en
# May 2019
###.............................................................................
#GOAL:
#DESCRIPTION:
#PROJECT: https://github.com/csmiguel/smallmammals_Kinabalu
###.............................................................................
#  REQUIRED FILES:
#   Description:
#   Inpath:
#  OUTPUT:
#    Description:
#    Outpath:
#  DEPENDENCIES:
###.............................................................................
input <- "data/intermediate/species_matrix.rds"
ecol <- readRDS(input)

#1. Dissimilarity index
  #Bray_Curtis
bc_b <- vegan::vegdist(ecol, "bray")
  #Jaccard
bc_jc <- vegan::vegdist(ecol, "jaccard")

#2. Clustering
cl <- hclust(bc_b, method = "average")
pdf("output/clustering.pdf", height = 5.5, width = 5.5 * 1.3)
plot(cl, main = "Bray_curtis distance UPGMA")
dev.off()

saveRDS(bc_b, "data/intermediate/bray_curtis_distance.rds")
