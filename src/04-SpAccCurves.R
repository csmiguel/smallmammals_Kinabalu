###.............................................................................
# (c) Miguel Camacho Sánchez
# miguelcamachosanchez@gmail.com // miguelcamachosanchez.weebly.com
# https://scholar.google.co.uk/citations?user=1M02-S4AAAAJ&hl=en
# May 2019
###.............................................................................
#GOAL: Species-Accumulation curves
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

input1 <- "data/intermediate/animals.rds"
animals <- readRDS(input1)

input2 <- "data/intermediate/traps.rds"
traps <- readRDS(input2)


#remove pitfall and tree traps
traps %<>%
  mutate(Trap_type = as.character(Trap_type)) %>%
  mutate(Habitat = as.character(Habitat)) %>%
  #replace NA in traptype and habitat for unknown. The reason is that whenever
  #the habitat was tree or trap was pitfall, it was for sure written down.
  replace_na(list(Trap_type = "unknown", Habitat = "unknown")) %>%
  filter(Trap_type != "pitfall" & Habitat != "tree") %>%
  mutate(Trap_type = as.factor(Trap_type)) %>%
  mutate(Habitat = as.factor(Habitat)) %>%
  mutate(Transect_altitude = as.factor(
    as.numeric(as.character(Transect_altitude)))) %>%
    dplyr::as_tibble()

#Animals
animals %<>%
  mutate(Trap_type = as.character(Trap_type)) %>%
  mutate(Habitat = as.character(Habitat)) %>%
  #replace NA in traptype and habitat for unknown. The reason is that whenever
  #the habitat was tree or trap was pitfall, it was for sure written down.
  replace_na(list(Trap_type = "unknown", Habitat = "unknown")) %>%
  filter(Trap_type != "pitfall" & Habitat != "tree") %>%
  #remove these duplicated columns from animals
  select(-Habitat, -Trap_type) %>%
  left_join(select(traps, TrapID, Date_set), by = "TrapID") %>%
  mutate(Date_collected = as.Date(Date_collected)) %>%
  mutate(Date_set = as.Date(Date_set)) %>%
  mutate(Transect_altitude = as.factor( #sort levels Transect_altitude
    as.numeric(as.character(Transect_altitude)))) %>%
    ##which trap night collected:
  mutate(trapnight = Date_collected - Date_set) %>%
    #Some animals were caught on the same day the trap was set:
  mutate(trapnight = gsub(0, 1, trapnight)) %>%
    # 1 M.alticola was caught in a prebaited trap
  mutate(trapnight = gsub(-1, 1, trapnight)) %>%
    # one S. everetti was caught in an open trap
  mutate(trapnight = as.numeric(gsub(17, 11, trapnight))) %>%
  dplyr::as_tibble()

############### BLOCK 2 ############
#make table of cumulative trapnighs
maxtp <- max(traps$Trap_nights, na.rm = T)
acc_tn <- traps %>%
  #add site:
  mutate(Site = as.factor(paste0(Location, "_", Transect_altitude))) %>%
  plyr::ddply("Site", function(x){ #for each Site:
    h <- numeric(maxtp)
    z <- 1:maxtp %>% sapply(function(y) {
      h[y] <- x[["Trap_nights"]] >= y #vector with T/F with T length equal to TN
    }
    ) %>% apply(2, sum) %>% cumsum() #sum T per column and cumlative sum
  })

#make table of cumulative trapnighs
acc_sp <- animals %>%
  #add site:
  mutate(Site = as.factor(paste0(Location, "_", Transect_altitude))) %>%
  plyr::ddply("Site", function(x){ #for each Site:
    h <- numeric(maxtp)
    v <- x %>%
      plyr::ddply("Species", function(y){ #for each Species
      min(y[["trapnight"]], na.rm = T)
    })
    seq_along(h) %>%
      sapply(function(z) h[z] <- sum(as.numeric(v$V1) <= z, na.rm = T))
  })

####Species accumulation plots
palette1 <- c("black", "#8c510a", "#d8b365", "#c7eae5", "#5ab4ac", "#01665e")

pdf(file = "output/trapping_effort.pdf", width = 8, height = 4)
mt <- c("Kinabalu", "Tambuyukon")
par(mfrow = c(1, 2))
maxy <- max(acc_sp[, -1], na.rm = T)
maxx <- max(acc_tn[, -1], na.rm = T)
seq_along(mt) %>%
  sapply(function(y){
    tn <- acc_tn %>% filter(grepl(mt[y], Site)) %>%
      mutate(elevation = as.numeric(
        gsub(paste0(mt[y], "_"), "", Site))) %>%
      arrange(desc(elevation))
    tns <- select(tn, -elevation, -Site)
    sp <- acc_sp %>% filter(grepl(mt[y], Site))  %>%
      mutate(elevation = as.numeric(
        gsub(paste0(mt[y], "_"), "", Site))) %>%
      arrange(desc(elevation))
    sps <- select(sp, -elevation, -Site)
    plot(as.numeric(tns[1, ]),
         as.numeric(sps[1, ]),
         xlab = "Cumulative trap-nights",
         ylab = "Species richness", type = "n",
         xlim = c(0, maxx + 40),
         ylim = c(0, maxy + 1),
         xaxs = "i", yaxs = "i")
    title(paste0("Mt. ", mt[y]), cex.main = .9)
    1:nrow(sp) %>% sapply(function(x){
      lines(as.numeric(tns[x, ]),
            as.numeric(sps[x, ]),
            type = "o", pch = x, lwd = 1.5, col = palette1[x], cex = .7)
    })
    legend("bottomright", legend = tn[["elevation"]],
           pch = 1:nrow(sp), col = palette1, bty = "n",
           lty = 1, pt.cex = .7, cex = .8)
  })
dev.off()
