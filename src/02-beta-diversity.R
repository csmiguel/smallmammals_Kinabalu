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
library(magrittr)

input <- "data/intermediate/species_matrix.rds"
ecol <- readRDS(input)
input2 <- "data/intermediate/endemics.rds"
endemics <- readRDS(input2)
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
#1. Bootstrap across species
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

#2. Bootstrap within sites
site_perm <- seq_along(mt) %>%
  lapply(function(x){
    z <- ecol1 %>%
    dplyr::mutate(elev = as.numeric(gsub("^.*_", "", rowname))) %>%
    dplyr::filter(location == mt[x]) %>%
    dplyr::select(-location, -rowname) %>%
    tibble::column_to_rownames("elev") %>% .[, colSums(.) > 0]
    1:1000 %>%
sapply(function(y){
  z %>%
      apply(1, function(x){
      sp_site <- x[x == 1]
      sp_site[names(sp_site) %in% endr] <- 0
      h <- sample(sp_site, length(sp_site), replace = T)
      1 - mean(h)
    })
  }) %>% apply(1, quantile, probs = c(0.025, 0.975))
})
names(site_perm) <- mt
cols <- c("blue", "orange")

pdf("output/endemism_elevation.pdf", width = 4 * 1.3, height = 4)
  plot(1, type = "n", xlim = c(500, 3300), ylim = c(0, max(unlist(n_end))),
      ylab = "Proportion of endemic species", xlab = "Elevation (m)",
      yaxt = "n")
  for (i in seq_along(mt)){
    nn <- names(n_end[[mt[i]]])
    pp <- site_perm[[mt[i]]]
    lines(nn, n_end[[mt[i]]], col = cols[i], type = "o", pch = c(15, 17)[i])
    polygon(c(nn, rev(nn)), c(pp[1, ], rev(pp[2, ])),
    col = adjustcolor(cols[i], alpha = 0.2), border = NA)
  }
  axis(2, at = seq(0, 1, 0.2), labels = seq(0, 1, 0.2), las = 2)
  legend("bottomright", legend = sapply(mt, function(x) paste0("Mt. ", x)),
    pch = c(15, 17), col = cols, bty = "n", lty = 1)
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

#beta multi with and without endemics
namesb <- c("beta.SIM", "beta.SNE",  "beta.SOR")
  #beta div on all sp matrix

b_all <- seq_along(mt) %>%
  lapply(function(x){
    ecol1 %>% filter(location == mt[x]) %>%
      dplyr::select(-rowname, -location) %>% .[, colSums(.) > 0] %>%
      betapart::beta.multi() %>% unlist() %>% as.data.frame() %>%
      rename(all = ".") %>% t()
    })
names(b_all) <- mt
  #beta only on endemics
b_Non_end <- seq_along(mt) %>%
  lapply(function(x){
    ecol1 %>% filter(location == mt[x]) %>%
      dplyr::select(-rowname, -location) %>%
      dplyr::select(-endr) %>% .[, colSums(.) > 0] %>%
      betapart::beta.multi() %>% unlist() %>% as.data.frame() %>%
      rename(Non_endemics = ".") %>% t()
    })
names(b_Non_end) <- mt

    #beta permutations
nperm <- 5000
b_perm <- seq_along(mt) %>%
  lapply(function(x){
    1:nperm %>% sapply(function(y){
    ecol1 %>% filter(location == mt[x]) %>%
      dplyr::select(-rowname, -location) %>%
      .[, colSums(.) > 0] %>%
      {sp <<- ncol(.); ensp <<- endr[endr %in% names(.)]; .} %>%
      .[, sample(sp, sp - length(ensp), replace = F)] %>%
      betapart::beta.multi()
    }
  ) %>% apply(1, unlist) %>%
        {quant <<- apply(., 2, quantile, probs = c(0.025, 0.5, 0.975)); .} %>%
        rbind(b_Non_end[[x]], .) %>%
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

##save output
saveRDS(bc_b, "data/intermediate/bray_curtis_distance.rds")
saveRDS(beta_endemics, "data/intermediate/beta_endemics.rds")
