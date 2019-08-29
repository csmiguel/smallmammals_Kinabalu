###.............................................................................
# (c) Miguel Camacho Sánchez
# miguelcamachosanchez@gmail.com // miguelcamachosanchez.weebly.com
# https://scholar.google.co.uk/citations?user=1M02-S4AAAAJ&hl=en
# May 2019
###.............................................................................
#DESCRIPTION: endemicity
#PROJECT: https://github.com/csmiguel/smallmammals_Kinabalu
###.............................................................................

library(dplyr)

input <- "data/intermediate/species_matrix.rds"
ecol <- readRDS(input)
input2 <- "data/intermediate/endemics.rds"
endemics <- readRDS(input2)

source("src/parameters/params.r")

rep0 <- function(x) {x[x > 0] <- 1; x}
perm <- 1000
##
ecol1 <- ecol %>%
  tibble::rownames_to_column() %>%
  dplyr::mutate(elev = as.numeric(gsub("^.*_", "", rowname))) %>%
  dplyr::mutate(location = gsub("_.*$", "", rowname)) %>%
  dplyr::arrange(location, elev) %>%
  dplyr::select(-"Suncus sp.", -"Crocidura sp")

#1. Bootstrap endemic species within site
# 1.1. Proportion of endemic species
n_end <- seq_along(mt) %>%
  lapply(function(x){
  ecol1 %>%
  dplyr::filter(location == mt[x]) %>%
  dplyr::select(-location, -rowname) %>%
  tibble::column_to_rownames("elev") %>%
  rep0 %>% #otherwise I would be calculating endemics over total catches
  {sp <<- rowSums(.); .} %>%
  dplyr::select(endemics) %>% rowSums() / sp
})
names(n_end) <- mt

# 1.2. Bootstrap within sites
site_perm <- seq_along(mt) %>%
  lapply(function(x){
    z <- ecol1 %>%
    dplyr::mutate(elev = as.numeric(gsub("^.*_", "", rowname))) %>%
    dplyr::filter(location == mt[x]) %>%
    dplyr::select(-location, -rowname) %>%
    tibble::column_to_rownames("elev") %>% .[, colSums(.) > 0]
    1:perm %>%
sapply(function(y){
  z %>%
      apply(1, function(w){
      h <- w[w > 0]
      h[h > 0] <- 1
      h[names(h) %in% endemics] <- 0
      #core bootstrapping function
      sample(h, length(h), replace = T) %>% as.numeric %>% {1- mean(.)}
    })
  }) %>% apply(1, quantile, probs = c(0.025, 0.5, 0.975))
})
names(site_perm) <- mt

#2. Bootstrap catches per trapping location
# 2.1. Proportion of endemic catches
end_catches <-
  seq_along(mt) %>%
  lapply(function(x){
  ecol1 %>%
  dplyr::filter(location == mt[x]) %>%
  dplyr::select(-location, -rowname) %>%
  tibble::column_to_rownames("elev") %>%
  {sp <<- rowSums(.); .} %>%
  dplyr::select(endemics) %>% rowSums() / sp
})
names(end_catches) <- mt

# 2.2. Bootstrap catches within sites
perm_catches <- seq_along(mt) %>%
  lapply(function(x){
    #1.select matrix for a given mountain
    z <- ecol1 %>%
    dplyr::filter(location == mt[x]) %>%
    dplyr::select(-location, -rowname) %>%
    tibble::column_to_rownames("elev") %>%
    .[, colSums(.) > 0]
    #2.run perm permutations
    1:perm %>%
sapply(function(y){
    #3.for each trapping location
  z %>%
  apply(1, function(h){
    end01 <- names(h) %in% endemics %>% as.numeric
    seq_along(end01) %>%
      sapply(function(x) rep(end01[x], h[x])) %>%
      unlist %>%
      #core command
      {sample(., size = sum(h), replace = T)} %>%
      mean
    })
  }) %>% apply(1, quantile, probs = c(0.025, 0.5, 0.975)) #get quantiles
})
names(perm_catches) <- mt

#3. fit model
  #3.1 for prop of endemics
  #create input for model
input_model <-
  reshape::melt(n_end) %>%
  dplyr::mutate(elev = seq_along(n_end) %>% sapply(function(x)
    c(names(n_end[[x]]))) %>% as.vector() %>% as.numeric) %>%
  dplyr::rename(prop_end = value) %>%
  dplyr::rename(location = L1) %>%
  dplyr::mutate(location = as.factor(location))
  #fit models
m1 <- lme4::glmer(prop_end ~ elev + (1 | location),
  data = input_model, family = binomial)
m2 <- lme4::glmer(prop_end ~ (1 | location),
  data = input_model, family = binomial)
  #output results from models to file
sink("output/endemism_models.txt")
cat("\nFull model fitted:\n")
summary(m1)
cat("\nNull model fitted:\n")
summary(m2)
cat("\nComparison between m1 and m2:\n")
anova(m1, m2)
sink()
#predict values
output_model <- input_model %>% mutate(fitted = boot::inv.logit(predict(m1)))
#3.2 for proportion of cathces
input_model_catches <-
  reshape::melt(end_catches) %>%
  dplyr::mutate(elev = seq_along(n_end) %>% sapply(function(x)
    c(names(n_end[[x]]))) %>% as.vector() %>% as.numeric) %>%
  dplyr::rename(prop_end = value) %>%
  dplyr::rename(location = L1) %>%
  dplyr::mutate(location = as.factor(location))
  #fit models
m21 <- lme4::glmer(prop_end ~ elev + (1 | location),
  data = input_model_catches, family = binomial)
m22 <- lme4::glmer(prop_end ~ (1 | location),
  data = input_model_catches, family = binomial)
  #output results from models to file
sink("output/endemism_models.txt")
cat("\n1. Proportion of endemic species:\n")
cat("\nFull model fitted:\n")
summary(m1)
cat("\nNull model fitted:\n")
summary(m2)
cat("\nComparison between m1 and m2:\n")
anova(m1, m2)
cat("\n2. Proportion of catches from endemic species:\n")
cat("\nFull model fitted:\n")
summary(m21)
cat("\nNull model fitted:\n")
summary(m22)
cat("\nComparison between m1 and m2:\n")
anova(m21, m22)
sink()

#save objects
saveRDS(list(m1 = m1, m2 = m2, m21 = m21, m22 = m22),
 "data/intermediate/models_endemicity.rds")
save(n_end, site_perm, end_catches, perm_catches,
  file = "data/intermediate/plot_endemics.Rdata")
