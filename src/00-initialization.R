###.............................................................................
# (c) Miguel Camacho Sánchez
# miguelcamachosanchez@gmail.com // miguelcamachosanchez.weebly.com
# https://scholar.google.co.uk/citations?user=1M02-S4AAAAJ&hl=en
# May 2019
###.............................................................................
#DESCRIPTION:
#1. read data from mysql database and creates an excel formatted file for
# supplementary file.
#2. create R rds files with raw data for downstream analysis
#3. create folder structure for repo
#PROJECT: https://github.com/csmiguel/smallmammals_Kinabalu
###.............................................................................
#1. create excel file with raw data
sf1 <- "data/raw/SF1.xlsx"
if (!file.exists(sf1)){ #create SF1 if it does not exists
mydb <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = 'Field', user = 'root',
  password = '', host = 'localhost')

#read all animals from sql database
animals <- DBI::dbGetQuery(mydb, "SELECT Animals.FieldCode,
Animals.ID as Species, Animals.TrapID, Traps.Location, Traps.Latitude,
Traps.Longitude, Traps.DEM_elevation,
Traps.Transect_altitude,
Animals.Date_collected, Traps.Trap_type,
Traps.habitat_simplified as 'Habitat', Animals.Collector,
Animals.Collected as 'Specimen_collected'
FROM Animals, Traps WHERE Animals.TrapID = Traps.TrapID")

animals$Specimen_collected <- plyr::mapvalues(animals$Specimen_collected,
      c(1, 0), c("yes", "no"))
#read all traps from sql database
traps <- DBI::dbGetQuery(mydb, "SELECT TrapID, Transect, Trap_number, Trap_type,
  Latitude, Longitude, Date_set, Date_closed, Location,
  habitat_simplified as 'Habitat', DEM_elevation, Trap_nights,
  Transect_altitude FROM Traps")
traps$Trap_type[traps$Trap_type == ""] <-  NA

#create file for SF1
file.copy("data/raw/variables_SF1.xlsx", sf1, overwrite = T)
#append animals sheet
xlsx::write.xlsx(animals, file = sf1, row.names = F,
  sheetName = "TableS1.1.AnimalsSampled", append = T)
#append traps sheet
xlsx::write.xlsx(traps, file = sf1, row.names = F,
  sheetName = "TableS1.2.TrappingEffort", append = T)
} else (print("SF1 already exists"))

rm(mydb, animals, traps)

#2. save raw data as R objects for further use
  animals <- xlsx::read.xlsx(sf1, sheetIndex = 2)
  animals$TrapID <- as.character(animals$TrapID)
  animals$FieldCode<- as.character(animals$FieldCode)
  #also  readxl::read_excel(sf1, sheet = 2)
  traps <- xlsx::read.xlsx(sf1, sheetIndex = 3)
  traps$TrapID <- as.character(traps$TrapID)

saveRDS(animals, "data/intermediate/animals.rds")
saveRDS(traps, "data/intermediate/traps.rds")

###endemic species to Borneo
endemics <- c("Melogale everetti", "Chiropodomys pusillus", "Maxomys alticola",
              "Maxomys ochraceiventer", "Niviventer rapit", "Rattus baluensis",
              "Sundasciurus everetti", "Sundasciurus jentinki",
              "Tupaia longipes", "Tupaia montana")
saveRDS(endemics, "data/intermediate/endemics.rds")

#3. create folder structure for repo
dir.create("output")
dir.create("data/intermediate")
