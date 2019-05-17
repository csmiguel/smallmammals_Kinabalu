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
library(dplyr)

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

#turnonver and nestedness components of beta diversity
ecol[ecol > 0] <- 1
ecol1 <- ecol %>%
  tibble::rownames_to_column() %>%
  dplyr::mutate(elev = as.numeric(gsub("^.*_", "", rowname))) %>%
  dplyr::mutate(location = gsub("_.*$", "", rowname)) %>%
  dplyr::arrange(location, elev) %>% dplyr::select(-elev)

endemics <- c("Melogale everetti", "Chiropodomys pusillus", "Maxomys alticola",
              "Maxomys ochraceiventer", "Niviventer rapit", "Rattus baluensis",
              "Suncus hosei", "Sundasciurus everetti", "Sundasciurus jentinki",
              "Tupaia longipes", "Tupaia montana")
ecolend <- ecol1[, match(c(endemics, "location", "rowname"),
  gsub("\\.", " ", names(ecol1)))]
ecolNend <- ecol1[, -match(endemics, gsub("\\.", " ", names(ecol1)))]

mt <- c("Kinabalu", "Tambuyukon")
l <- list(ecol = ecol1, ecolend = ecolend, ecolNend = ecolNend)
h <- seq_along(l) %>%
  plyr::llply(function(y){
    z <-  l[[y]]
    p <- seq_along(mt) %>%
      lapply(function(x){
         z %>% filter(location == mt[x]) %>%
          dplyr::select(-location) %>%
          tibble::column_to_rownames() %>%
          betapart::beta.pair()
      })
  names(p) <- mt
  p
})
names(h) <- names(l)


#proportion of endemic species
endr <- gsub(" ", ".", endemics)
n_end <- seq_along(mt) %>%
  lapply(function(x){
  ecol1 %>%
  dplyr::mutate(elev = as.numeric(gsub("^.*_", "", rowname))) %>%
  dplyr::filter(location == mt[x]) %>%
  dplyr::select(-location, -rowname) %>%
  tibble::column_to_rownames("elev") %>% {sp <<- rowSums(.); .} %>%
  dplyr::select(endr) %>% rowSums() / sp
})
names(n_end) <- mt
#permutations
perm_end <- seq_along(mt) %>%
  lapply(function(x){
    z <- ecol1 %>%
    dplyr::mutate(elev = as.numeric(gsub("^.*_", "", rowname))) %>%
    dplyr::filter(location == mt[x]) %>%
    dplyr::select(-location, -rowname) %>%
    tibble::column_to_rownames("elev") %>% .[, colSums(.) > 0]
    1:1000 %>%
sapply(function(y){
  z %>% {sp <<- rowSums(.); .} %>%
  .[, sample(ncol(z), sum(endr %in% names(z)), replace = F)] %>% rowSums() / sp
  }) %>% apply(1, quantile, probs = c(0.025, 0.975))
})
names(perm_end) <- mt
cols <- c("blue", "orange")

plot(1, type = "n", xlim = c(500, 3300), ylim = c(0, max(unlist(n_end))),
    ylab = "Endemic species", xlab = "Elevation")
for (i in seq_along(mt)){
  nn <- names(n_end[[mt[i]]])
  pp <- perm_end[[mt[i]]]
  lines(nn, n_end[[mt[i]]], col = cols[i], type = "b")
  polygon(c(nn, rev(nn)), c(pp[1, ], rev(pp[2, ])),
  col = adjustcolor(cols[i], alpha = 0.2), border = NA)
}

##save output
saveRDS(bc_b, "data/intermediate/bray_curtis_distance.rds")
