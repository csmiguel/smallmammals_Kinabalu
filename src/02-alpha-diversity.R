###.............................................................................
# (c) Miguel Camacho Sánchez
# miguelcamachosanchez@gmail.com // miguelcamachosanchez.weebly.com
# https://scholar.google.co.uk/citations?user=1M02-S4AAAAJ&hl=en
# May 2019
###.............................................................................
#GOAL: calculate indices of alpha diversity
#DESCRIPTION:
#PROJECT: https://github.com/csmiguel/smallmammals_Kinabalu
###.............................................................................

library(dplyr)
animals <- readRDS("data/intermediate/animals.rds")

#create new variable concatenating Location and elevation
animals <- animals %>% mutate(Site = as.factor(paste(Location,
    Transect_altitude, sep = "_")))

#1. Estimate alpha diversity
# create matrix for species diversity with columns being species and rows being
# locations. The number of catches are in each cell.

ecol <- with(animals, table(Site, Species))

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
write.csv(file = "output/TableS2_alpha_diversity.csv", row.names = F)

saveRDS(as.data.frame.matrix(ecol), "data/intermediate/species_matrix.rds")
saveRDS(div, "data/intermediate/alpha_diversity.rds")
