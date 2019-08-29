###.............................................................................
# (c) Miguel Camacho Sánchez
# miguelcamachosanchez@gmail.com // miguelcamachosanchez.weebly.com
# https://scholar.google.co.uk/citations?user=1M02-S4AAAAJ&hl=en
# May 2019
###.............................................................................
#DESCRIPTION: Plot species per elevation.
# Create figure of mammal distribution in different altitudes as
# in Fig 3 Heaney 2011. Compare our data with Nor data.
#PROJECT: https://github.com/csmiguel/smallmammals_Kinabalu
###.............................................................................

library(dplyr)
#read data from Nor 2001:
input1 <- "data/raw/Nor_data.csv"
nor <- read.csv(input1, sep = " ") %>%
        mutate(Location = rep("Nor", length(Species))) %>%
        dplyr::rename(Elevation = Altitude) %>%
        dplyr::rename(FieldCode = Number) %>%
        as_tibble()
#our data
input2 <- "data/intermediate/animals.rds"
animals <- readRDS(input2) %>%
            dplyr::select(FieldCode, Species, Location, DEM_elevation) %>%
            dplyr::rename(Elevation = DEM_elevation) %>%
            dplyr::as_tibble()
input3 <- "data/intermediate/endemics.rds"
endemics <- readRDS(input3)

source("src/parameters/params.r")
#---------Create combined figure from ours and Nor results------------

# 1. Create combined dataframe for our's and Nor's data
comb <- rbind(animals, nor)
# sort levels species by Family:
comb %<>%
  mutate(Group =
    case_when(
      grepl("Callosciurus", Species)~"Sciuridae",
      grepl("Chiropodomys", Species)~"Muridae",
      grepl("Crocidura", Species)~"Eulipotyphla",
      grepl("Hylomys", Species)~"Eulipotyphla",
      grepl("Lenothrix", Species)~"Muridae",
      grepl("Leopoldamys", Species)~"Muridae",
      grepl("Maxomys", Species)~"Muridae",
      grepl("Melogale", Species)~"Carnivora",
      grepl("Niviventer", Species)~"Muridae",
      grepl("Rattus", Species)~"Muridae",
      grepl("Suncus", Species)~"Eulipotyphla",
      grepl("Sundamys", Species)~"Muridae",
      grepl("Sundasciurus", Species)~"Sciuridae",
      grepl("Tupaia", Species)~"Tupaiidae")
  ) %>% arrange(Group, Species) %>%
  mutate(Species = factor(Species, levels(Species)[#reorder levels
    match(as.character(unique(Species)), levels(Species))])) %>%
    mutate(num_sp = 1:nrow(comb))

#PLOTs
pdf(file = "output/Figure2_SpeciesDistribution.pdf", width = 11, height = 6)
par(mar = c(8, 4.5, 2, 1))
#plot xy limits
llim <- 200
ulim <- 3500
spn <- as.numeric(comb$Species)
#empty plot with correct dimensions
plot(spn, comb$Elevation, ylim = c(llim, ulim),
     xlim = c(0.5, max(spn) - 0.3), xaxt = "n",
     ylab = "Elevation (m)", yaxt = "n", xlab = "", type = "n",
     xaxs = "i", yaxs = "i")
#Kitayama vegetation levels
assertthat::assert_that(ulim < 3800 & llim > 0)
kitayama_elev <- c(llim, 1200, 2000, 2800, ulim)
kitayama_name <- c("lowland", "lower montane", "upper montane", "subalpine")
#plot grey polygons as Kitayama's elevation levels
for (i in 2:4){
  polygon(x = c(0, rep(nlevels(comb$Species), 2), 0),
          y = c(rep(kitayama_elev[i], 2), rep(kitayama_elev[i + 1], 2)),
          col = grey(1 - (i ^ 1.5 / 30)), border = NA)
        }

#list to withdraw parameters for lines and points
h <- list(loc = c(mt, "Nor"),
      pchm = c(16, 17, 22),
      ptp = c(0, 0.2, -0.2),
      colp = c(cols, "black"))
#draw points and lines
for (i in seq_along(levels(comb$Location))){
  #for each location
  combm <- filter(comb, Location == h$loc[i])
  points(as.numeric(combm$Species) + h$ptp[i],
    combm$Elevation, pch = h$pchm[i],
    col = h$colp[i])
  #for each species within each location
  for (j in seq_along(levels(combm$Species))){
    an <- filter(combm, Species == levels(combm$Species)[j])
    if (nrow(an) > 0)
    lines(c(j + h$ptp[i], j + h$ptp[i]),
      c(min(an$Elevation), max(an$Elevation)))
    }
  }

#x axis: species labels

assertthat::assert_that(all(endemics %in% levels(comb$Species)))
  #create vector with font italics and bold italics for endemics
  font_sp <- rep(3, nlevels(comb$Species)) %>%
            {.[match(endemics, levels(comb$Species))] <- 4; .}
axis(1, at = (1:nlevels(comb$Species)),
     labels = rep("", nlevels(comb$Species), las = 2))
#font does not allows vectors, so I have to use an apply function
seq_along(font_sp) %>% sapply(function(x){
axis(1, at = x,
     labels = levels(comb$Species)[x], las = 2, cex.axis = 0.7,
     font = font_sp[x], lty = 0)
   })

#Elevation labels
elev <- c(500, 1000, 1500, 2000, 2500, 3000)
axis(2, at = elev, labels = as.character(elev), las = 2, cex.axis = 0.8)

#Lines to separate species
for (i in seq_along(levels(comb$Species)) - 1) abline(v = i + 0.5, lty = 3)

#vegetations levels as in Kitayama 1992
text(nlevels(comb$Species) - 3, kitayama_elev[2:5] - 100,
  labels = kitayama_name, cex = 0.7)
legend(x = 9,
       y = 3300,
       legend = c(paste("Mt.", mt), paste0(h$loc[3], ", 2001")),
       pch = h$pchm,
       bg = adjustcolor("white", alpha = 0.5),
       col = c(cols, "black"))

dev.off()
