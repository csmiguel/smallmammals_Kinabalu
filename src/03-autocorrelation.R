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
input3 <- "data/intermediate/rangemodel.rds"
rmod <- readRDS(input3)

#create new variable concatenating Location and elevation
animals <- animals %>% dplyr::mutate(Site = as.factor(paste(animals$Location,
          animals$Transect_altitude, sep = "_")))
#get centroid coordinates for each trapping location
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
input_autocorr <- dplyr::left_join(div, coords, by = c("Site", "Site")) %>%
    dplyr::arrange(Location, Elevation)

#include geometric constraints as a variable
# convert results from rangemodel from a list to a dataframe
rangemodel <-
seq_along(rmod) %>% plyr::ldply(function(x) {
  rmod[[x]] %>%
   select(mean, class) %>% mutate(mt = names(rmod[x]))
   })
#calculate the species richness for the closest elevation from rangemodel
  input_autocorr2 <-
  input_autocorr %>% plyr::ddply("Location", function(x){
    mount <- x$Location[1]
    seq_along(x$Location) %>% sapply(function(y){
      elev <- x$Elevation[y]
      rmdel <- rangemodel %>% filter(mt == mount)
      minv <- abs(rmdel$class - elev)
      mean_sr <- rmdel[which(minv == sort(minv)[1]), ]$mean %>% mean
      c(mount, elev, mean_sr)
    }
    ) %>% t
  }
  ) %>% dplyr::rename(Elevation = "2") %>%
      dplyr::rename(mde_richness = "3") %>%
      dplyr::select(-"1") %>%
  #join to input_autocorr
dplyr::left_join(input_autocorr, ., by = c("Location", "Elevation"))

#I cannot compare them with anova
#lmer: model to include random effects and spatial autocorrelation
m1 <- nlme::lme(Richness ~ Elevation + mde_richness, random = ~1 | Location,
  data = input_autocorr2, correlation = corSpatial(form = ~ x + y),
  method = "ML")
m2 <- nlme::lme(Richness ~ Elevation, random = ~1 | Location,
  data = input_autocorr2, correlation = corSpatial(form = ~ x + y),
  method = "ML")
m3 <- nlme::lme(Richness ~ mde_richness, random = ~1 | Location,
  data = input_autocorr2, correlation = corSpatial(form = ~ x + y),
  method = "ML")
m4 <- nlme::lme(Richness ~ 1, random = ~1 | Location,
  data = input_autocorr2, correlation = corSpatial(form = ~ x + y),
  method = "ML")

#Plot results
all_models <- list(m1 = m1, m2 = m2, m3 = m3, m4 = m4)

pred <- seq_along(all_models) %>% sapply(function(x){
  predict(all_models[[x]])
  }
) %>% as.data.frame
names(pred) <- paste0("pred_", names(all_models))
res <- seq_along(all_models) %>% sapply(function(x){
  residuals(all_models[[x]])
  }
) %>% as.data.frame
names(res) <- paste0("res_", names(all_models))

results <- cbind(input_autocorr2, pred, res)

#save results to file
sink("output/models_alpha_diversity.txt")
for (i in seq_along(all_models)){
  cat("\n\n\nNext model\n")
  cat(names(all_models)[i], "\n\n")
  print(summary(all_models[[i]]))
}
cat("\n\n\nANOVA\n\n\n")
anova(m1, m2)
anova(m1, m3)
anova(m1, m4)
anova(m2, m4)
anova(m3, m4)
sink()

#save data
saveRDS(results, "data/intermediate/models_alpha_diversity.rds")
