###.............................................................................
# (c) Miguel Camacho Sánchez
# miguelcamachosanchez@gmail.com // miguelcamachosanchez.weebly.com
# https://scholar.google.co.uk/citations?user=1M02-S4AAAAJ&hl=en
# May 2019
###.............................................................................
#GOAL: beta diversity. Overall and pairwise measures
#DESCRIPTION:
#PROJECT: https://github.com/csmiguel/smallmammals_Kinabalu
###.............................................................................

library(dplyr)
library(magrittr)

input <- "data/intermediate/species_matrix.rds"
ecol <- readRDS(input)
input2 <- "data/intermediate/endemics.rds"
endemics <- readRDS(input2)
source("src/parameters/params.r")

#1. tranformations for the ecology matrix
ecol[ecol > 0] <- 1
ecol1 <- ecol %>%
  tibble::rownames_to_column() %>%
  dplyr::mutate(elev = as.numeric(gsub("^.*_", "", rowname))) %>%
  dplyr::mutate(location = gsub("_.*$", "", rowname)) %>%
  dplyr::arrange(location, elev) %>% dplyr::select(-elev)
mt <- c("Kinabalu", "Tambuyukon") #mountains

#2. Pairwise beta diversity for all locations
# calculate sorensen distance between sites
sor <-
  ecol1 %>%
  dplyr::select(-location) %>%
  {sor_matrix <<- tibble::column_to_rownames(.)} %>%
  betapart::beta.pair()
# root tree
tk <- ape::nj(sor$beta.sor) %>%
  phangorn::midpoint(tk)
# write to file pairwise beta diversity
sink("output/sorensen_pairwise.txt")
sor
sink()

#3. Pairwise turnonver and nestedness components of beta diversity
# GOAL: compute turnonver and nestedness components of diversity from pairwise
# locations for each mountain

  #matrix with only endemics
ecolend <- ecol1[, match(c(endemics, "location", "rowname"), names(ecol1))]
  #matrix with non-endemics
ecolNend <- ecol1[, -match(endemics, names(ecol1))] %>%
            dplyr::select(-"Suncus sp.", -"Crocidura sp")
  #list with all ecol matrix
l <- list(all = ecol1, only_endemics = ecolend, non_endemics = ecolNend)
  #compute pairwise beta diversity between locations for each mountain
h <- seq_along(l) %>%
  plyr::llply(function(y){
    z <-  l[[y]]
    p <- seq_along(mt) %>%
      lapply(function(x){
         z %>% filter(location == mt[x]) %>%
          dplyr::select(-location) %>%
          dplyr::mutate(rowname = gsub("^.*_", "", rowname)) %>%
          tibble::column_to_rownames() %>%
          betapart::beta.pair() %>% lapply(round, 2)
      })
  names(p) <- mt
  p
})
names(h) <- names(l)
  #write ouput to file
sink("output/TableS3_pairwise_betadiv.txt")
h$all
sink()
rm(ecolend, l, ecolNend)

#4. Contribution of endemics to nestedness and turnover of beta diversity
# I will calculate (1) overall beta diversity and (2) beta diversity without
# endemic species. To evaluate the significancy of the nestedness and turnover
# components of beta diversity without endemics, I resample from the ecological
# matrix containing all species a number of columns = (number of columns -
# number of endemics). I do it without replacement because the concept is what
# is the effect of removing the endemics in beta diversity compared to removing
# random samples. I repeat this nperm times to estimate p values and
# quantiles.
  #vector with names of variables returned by beta.multi
namesb <- c("beta.SIM", "beta.SNE",  "beta.SOR")
  #beta diversity on matrix with all species
b_all <- seq_along(mt) %>%
  lapply(function(x){
    ecol1 %>% filter(location == mt[x]) %>%
      dplyr::select(-rowname, -location) %>% .[, colSums(.) > 0] %>%
      betapart::beta.multi() %>% unlist() %>% as.data.frame() %>%
      rename(all = ".") %>% t()
    })
names(b_all) <- mt
  #beta diversity on matrix with only endemics
b_Non_end <- seq_along(mt) %>%
  lapply(function(x){
    ecol1 %>% filter(location == mt[x]) %>%
      dplyr::select(-rowname, -location) %>%
      dplyr::select(-endemics, -"Suncus sp.", -"Crocidura sp") %>%
      .[, colSums(.) > 0] %>%
      betapart::beta.multi() %>% unlist() %>% as.data.frame() %>%
      rename(Non_endemics = ".") %>% t()
    })
names(b_Non_end) <- mt

    #beta diversity on permuted matrices removing
    # random species = length(endemics)
nperm <- 5000 #number of perm
b_perm <- seq_along(mt) %>%
  lapply(function(x){
    1:nperm %>% sapply(function(y){
    ecol1 %>% filter(location == mt[x]) %>%
      dplyr::select(-rowname, -location, -"Suncus sp.", -"Crocidura sp") %>%
      .[, colSums(.) > 0] %>% #removes especies not present in a given mt
      #stores sp as number of species in a given mt and ensp, number of endemics
      #in that given mt.
      {sp <<- ncol(.); ensp <<- endemics[endemics %in% names(.)]; .} %>%
      .[, sample(sp, sp - length(ensp), replace = F)] %>%
      betapart::beta.multi()
    }
  ) %>% apply(1, unlist) %>% {
    assign(paste0("all_perm", mt[x]), ., envir = .GlobalEnv) ; .} %>%
        #get quantiles
        {quant <<- apply(., 2, quantile, probs = c(0.025, 0.5, 0.975)); .} %>%
        #bind permuted values to values measured withouth Non_endemics
        rbind(b_Non_end[[x]], .) %>%
        #calculate p-values
        apply(2, function(y) sum(y[1] <= y[2:nperm]) / nperm) %>%
        rbind(., quant) %>%
        `rownames<-`(c("p_value", rownames(quant)))
    })
names(b_perm) <- mt
#bind all
  beta_endemics <-
    seq_along(mt) %>%
      lapply(function(x){
        rbind(b_all[[x]], b_Non_end[[x]], b_perm[[x]]) %>% round(3) %>%
  {write.table(., paste0("output/beta_endemics_", mt[x], ".txt")); .}
        })
  names(beta_endemics) <- mt
endemics_output <- list(summary = beta_endemics, permT = all_permTambuyukon,
  permK = all_permKinabalu)

##save output
saveRDS(endemics_output, "data/intermediate/betadiv_global.rds")
saveRDS(h, "data/intermediate/betadiv_pairwise.rds")
saveRDS(tk, "data/intermediate/nj_tree.rds")
