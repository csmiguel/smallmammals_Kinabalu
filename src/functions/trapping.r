#write table with Location,Transect_altitude,catches,effort,Trap_success
#for all traps or without tree and pitfalls.

source("src/function/read_data.r")
write_trapping <- function(m = c("all", "ground_traps")){
  traps <- read_traps(m = m)
  animals <- read_traps(m = m)
    N <- animals %>%
        plyr::ddply(c("Location", "Transect_altitude"), function(x){
          nrow(x)
        }) %>%
        dplyr::rename(catches = V1)
    tr <- traps %>%
      plyr::ddply(c("Location", "Transect_altitude"), function(x){
        sum(x$Trap_nights)
      }) %>%
        dplyr::rename(effort = V1)
    trap_success <-
        N %>%
        cbind(select(tr, effort)) %>%
        mutate(Trap_success = round(catches / effort, 3))
    outputp <- paste0("output/trap_success_", m, ".txt")
    write.csv(trap_success, outputp)
      }
