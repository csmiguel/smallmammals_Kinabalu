###.............................................................................
# (c) Miguel Camacho Sánchez
# miguelcamachosanchez@gmail.com // miguelcamachosanchez.weebly.com
# https://scholar.google.co.uk/citations?user=1M02-S4AAAAJ&hl=en
# May 2019
###.............................................................................
#GOAL: Plot species per elevation
#DESCRIPTION: Create figure of mammal distribution in different altitudes as
# in Fig 3 Heaney 2011. Compare our data with Nor data.
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
input2 <- "data/intermediate/endemics.rds"
endemics <- readRDS(input2) %>% gsub(" ", ".", .) #this replacement allows
  #proper matching with colnames in ecol.
mt <- c("Kinabalu", "Tambuyukon")
rep0 <- function(x) {x[x > 0] <- 1; x}
perm <- 100
##
ecol1 <- ecol %>%
  tibble::rownames_to_column() %>%
  dplyr::mutate(elev = as.numeric(gsub("^.*_", "", rowname))) %>%
  dplyr::mutate(location = gsub("_.*$", "", rowname)) %>%
  dplyr::arrange(location, elev)

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
    1:100 %>%
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
      sapply(function(x) rep(end01[x], h[x])) %>% unlist %>%
      {sample(., size = sum(h), replace = T)} %>%
      mean
    })
  }) %>% apply(1, quantile, probs = c(0.025, 0.5, 0.975)) #get quantiles
})
names(perm_catches) <- mt

#3. Plots
source("src/functions/plot_bootstrap_endemics.r")
pdf("output/endemism_elevation.pdf", width = 4 * 2.3, height = 4)
par(mfrow = c(1, 2))
# 3.1 Endemic species elevation (point 2)
  plot_bootstrap_endemics(n_end, site_perm, "Proportion of endemic species", mt)
# 3.2 Endemic catches elevation (point 2)
  plot_bootstrap_endemics(end_catches, perm_catches,
    "Proportion of endemic catches", mt)
dev.off()
#fit model
input_model <-
  reshape::melt(n_end) %>%
  dplyr::mutate(elev = seq_along(n_end) %>% sapply(function(x)
    c(names(n_end[[x]]))) %>% as.vector() %>% as.numeric) %>%
  dplyr::rename(prop_end = value) %>%
  dplyr::rename(location = L1) %>%
  dplyr::mutate(location = as.factor(location))

m1 <- lme4::glmer(prop_end ~ elev + (1 | location),
  data = input_model, family = binomial)
m2 <- lme4::glmer(prop_end ~ (1 | location),
  data = input_model, family = binomial)
anova(m1, m2)

table_model %<>% mutate(fitted_m1 = fitted(m1))
