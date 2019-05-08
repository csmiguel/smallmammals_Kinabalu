###.............................................................................
# (c) Miguel Camacho Sánchez
# miguelcamachosanchez@gmail.com // miguelcamachosanchez.weebly.com
# https://scholar.google.co.uk/citations?user=1M02-S4AAAAJ&hl=en
# May 2019
###.............................................................................
#GOAL: Test autocorrelation
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
library(sp)
library(nlme)
library(geosphere)

input1 <- "data/intermediate/animals.rds"
animals <- readRDS(input1)
input2 <- "data/intermediate/alpha_diversity.rds"
div <- readRDS(input2)

#create new variable concatenating Location and elevation
animals <- animals %>% mutate(Site = as.factor(paste(animals$Location,
          animals$Transect_altitude, sep = "_")))

coords <- animals %>% dplyr::select(Site, Longitude, Latitude) %>%
          plyr::ddply("Site", function(x) {
            x <- dplyr::select(x, Longitude, Latitude)
            x <- sp::SpatialPoints(x, proj4string = CRS("+init=epsg:4326"))
            x <- sp::spTransform(x, CRS("+init=epsg:32650"))
            x <- rgeos::gCentroid(x)
            x <- as.data.frame(x)
            names(x) <- c("x", "y")
            x
          }
        )

#create input for autocorrelation test
input_autocorr <- left_join(div, coords, by = c("Site", "Site"))

# fit model
full_model <- nlme::gls(Richness ~ Elevation, data = input_autocorr,
  method = "ML", correlation = nlme::corSpatial(form = ~ x + y))

# fit null model
null_model <- nlme::gls(Richness ~ 1, data = input_autocorr,
  method = "ML", correlation = nlme::corSpatial(form = ~ x + y))

# compare models
an <- anova(full_model, null_model)

# create data for trend line
pred_data <- data.frame(Elevation = seq(min(input_autocorr$Elevation),
                                       max(input_autocorr$Elevation),
                                       length.out = 1000))
pred_data$Richness <- predict(full_model, newdata = pred_data)
# plot results
plot(Richness ~ Elevation, data = input_autocorr)
lines(Richness ~ Elevation, data = pred_data, col = "red")

#save results to file
sink("output/autocorrelation.txt")
  cat("Autocorrelation test\n Full model")
  summary(full_model)
  cat("Null model")
  summary(null_model)
  cat("ANOVA comparing both models")
  an
sink()

# MLPE model
  #Jeff Hanson kindly suggested this approach and provided the code.
    #check reference 10.1002/sim.3429
  #it is suppose to deal with the non-independence of points from matrix
  #comparisons as in mantel tests.
library(ResistanceGA)
source("src/functions/mlpe.R")
input3 <- "data/intermediate/bray_curtis_distance.rds"
bc_b <- readRDS(input3)
m1 <- mlpe(y = dist(as.matrix(input_autocorr$x, input_autocorr$y)), x = bc_b)
#issue: interpret output
