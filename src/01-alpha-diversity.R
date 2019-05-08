###.............................................................................
# (c) Miguel Camacho Sánchez
# miguelcamachosanchez@gmail.com // miguelcamachosanchez.weebly.com
# https://scholar.google.co.uk/citations?user=1M02-S4AAAAJ&hl=en
# May 2019
###.............................................................................
#GOAL: alpha diversity
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
animals <- readRDS("data/intermediate/animals.rds")

#create new variable concatenating Location and elevation
animals <- animals %>% mutate(Site = as.factor(paste(Location,
    Transect_altitude, sep = "_")))

#1. Estimate alpha diversity
#create matrix for species diversity with columns as species and rows as
#locations with the number of catches at each cell

ecol <- data.frame(matrix(NA, nrow = nlevels(animals$Site),
  ncol = nlevels(animals$Species), dimnames = list(levels(animals$Site),
  levels(animals$Species))))

for (i in 1:length(levels(animals$Site))){
  for (j in 1:length(levels(animals$Specie))){
    ecol[i, j] <- nrow(animals[animals$Site == levels(animals$Site)[i] &
     animals$Species == levels(animals$Species)[j], ])
  }
}


#Shannon diversity
H <- vegan::diversity(ecol, "shannon")
#Simpson
D <- 1 - vegan::diversity(ecol, "simpson")
  #1-D, being D the probability of two individuals being
  #the same when withdrawn from the dataset
#Species richness
S <- apply(ecol, 1, function(x)sum(x > 0))

#diversity indexes per location
div <- data.frame(Site = names(H),
                  Location = gsub("_.*", "", names(D)),
                  Elevation = as.numeric(gsub(".*_", "", names(D))),
                  Shannon = round(H, 2),
                  Simpson = round(D, 2),
                  Richness = S,
                  Evenness = round(H / log(S), 2))
#write alpha diversity to csv
div %>% select(-Site) %>% arrange(Location, Elevation) %>%
write.csv(file = "output/alpha_diversity.csv", row.names = F)


#2. Plot alpha diversity
  #create objects from RangeModel results
  rmk <- read.csv("data/raw/RinputKinabalu5000.txt")#results from RangeModel
  rmt <- read.csv("data/raw/RinputTambuyukon5000.txt")#results from RangeModel
  #issue1: redo rangemodel with R script

  #ylim for species Richness
  max_s <- max(rmk$Computed.Upper.95.CI, rmt$Computed.Upper.95.CI, div$Richness)
  min_s <- 0

index <- c("Richness", "Shannon", "Simpson", "Evenness")
labs <- c("Species richness S", "Shannon H'", "Simpson D", "Evenness J'")
col_site <- c("blue", "orange")[div$Location]
source("src/functions/plot_div.r")
  #start plot
pdf("output/alpha_diversity.pdf", height = 4, width = 4 * 1.66)
par(mfrow = c(2, 2))
for (i in seq_along(index)){
  indexi <- index[i]
  if(indexi == "Richness"){
    plot(div$Elevation, div[indexi][, 1], col = col_site,
      pch = as.numeric(div$Location),
      ylab = labs[i], xlab = "",
      ylim = c(min_s, max_s))
    plot_rangemodel(rmk, "blue")
    plot_rangemodel(rmt, "orange")
  } else if (indexi != "Richness"){
  plot(div$Elevation, div[indexi][, 1], col = col_site,
    pch = as.numeric(div$Location),
    ylab = labs[i], xlab = "")
  }
  lowess_line("Kinabalu", indexi)
  lowess_line("Tambuyukon", indexi)
}
dev.off()#issue2: save all plots to same page.

saveRDS(ecol, "data/intermediate/species_matrix.rds")
saveRDS(div, "data/intermediate/alpha_diversity.rds")
