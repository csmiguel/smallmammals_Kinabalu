###.............................................................................
# (c) Miguel Camacho Sánchez
# miguelcamachosanchez@gmail.com // miguelcamachosanchez.weebly.com
# https://scholar.google.co.uk/citations?user=1M02-S4AAAAJ&hl=en
# May 2019
###.............................................................................
#DESCRIPTION: models geometric constraints. As in Wang and Fang 2012
#PROJECT: https://github.com/csmiguel/smallmammals_Kinabalu

library(dplyr)
library(rangemodelR)

animals <- readRDS("data/intermediate/animals.rds")

fc2nu <- function(x) as.numeric(as.character(x))
#pameters
boundr <- "ss" #kind of boundary. See range_shuffle function
int <- 50 #binning of min and max elevations
maxel <- max(animals$DEM_elevation) #max elevation
#calculate geometric constraints
rangemodel <-
animals %>%
  dplyr::select(Species, Location, DEM_elevation) %>%
  dplyr::rename(Elevation = DEM_elevation) %>%
  plyr::dlply("Location", function(y){
   y %>% plyr::ddply("Species", function(x){
     sbreaks <- seq(0, maxel + int, int)
     lab_breaks <- seq(int / 2, maxel + (int / 2), int)
  data.frame(min = fc2nu(cut(min(x$Elevation), sbreaks, lab_breaks)),
             max = fc2nu(cut(max(x$Elevation), sbreaks, lab_breaks)))
             }) %>%
  dplyr::mutate(range = max - min) %>%
  dplyr::mutate(mid = (max + min) / 2) %>%
  dplyr::select(-Species) %>%
  dplyr::mutate(num_zones = ceiling((range + 1) / int)) %>%
  #core function
  rangemodelR::range_shuffle(., boundary = boundr,
                                   interval = int, var = NULL,
                                   sites = seq(int / 2, maxel + int, int),
                                   reps = 5000, degen = FALSE) %>%
  dplyr::mutate(class = fc2nu(rownames(.))) %>%
   dplyr::arrange(class)
                                 })
saveRDS(rangemodel, "data/intermediate/rangemodel.rds")
