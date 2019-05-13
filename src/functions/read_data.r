read_traps <- function(m = c("all", "ground_only")){

input2 <- "data/intermediate/traps.rds"
traps <- readRDS(input2)
traps %<>%
  do(
    if (m == "ground_traps"){
      mutate(., Trap_type = as.character(Trap_type)) %>%
      mutate(Habitat = as.character(Habitat)) %>%
      replace_na(list(Trap_type = "unknown", Habitat = "unknown")) %>%
      filter(Trap_type != "pitfall" & Habitat != "tree") %>%
      mutate(Trap_type = as.factor(Trap_type)) %>%
      mutate(Habitat = as.factor(Habitat))
    }
      else .) %>%
  mutate(Transect_altitude = as.factor(
    as.numeric(as.character(Transect_altitude)))) %>%
    dplyr::as_tibble()
}

read_animals <- function(m = c("all", "ground_only")){
  traps <- read_traps(m = m)
  input1 <- "data/intermediate/animals.rds"
  animals <- readRDS(input1)
animals %<>%
    do(
      if (m == "ground_traps"){
        mutate(., Trap_type = as.character(Trap_type)) %>%
        mutate(Habitat = as.character(Habitat)) %>%
        replace_na(list(Trap_type = "unknown", Habitat = "unknown")) %>%
        filter(Trap_type != "pitfall" & Habitat != "tree")
        }
        else .) %>%
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
}
