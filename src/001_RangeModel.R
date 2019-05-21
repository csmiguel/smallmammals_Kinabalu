###.............................................................................
# (c) Miguel Camacho Sánchez
# miguelcamachosanchez@gmail.com // miguelcamachosanchez.weebly.com
# https://scholar.google.co.uk/citations?user=1M02-S4AAAAJ&hl=en
# May 2019
###.............................................................................
#GOAL: model geometric constraints
#DESCRIPTION:as in Wang and Fang 2012
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
# https://cran.r-project.org/web/packages/rangemodelR/rangemodelR.pdf
# https://onlinelibrary.wiley.com/doi/full/10.1111/j.1466-8238.2007.00286.x
# https://onlinelibrary.wiley.com/doi/full/10.1111/j.1466-8238.2006.00284.x
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0027975
# http://craigmcclain.com/Papers/McClain_OIKOS_2004.pdf
#10.1111/j.1600-0587.2012.07384.x

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
  rangemodelR::range_shuffle(., boundary = boundr,
                                   interval = int, var = NULL,
                                   sites = seq(int / 2, maxel + int, int),
                                   reps = 5000, degen = FALSE) %>%
  dplyr::mutate(class = fc2nu(rownames(.))) %>%
   dplyr::arrange(class)
                                 })
saveRDS(rangemodel, "data/intermediate/rangemodel.rds")
