###.............................................................................
# (c) Miguel Camacho Sánchez
# miguelcamachosanchez@gmail.com // miguelcamachosanchez.weebly.com
# https://scholar.google.co.uk/citations?user=1M02-S4AAAAJ&hl=en
# May 2019
###.............................................................................
#GOAL: Tables with trapping effort
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
  #Table with trapping sucess
  #variables: Mt, elevation, no. catches, trapnights, trap success

source("src/functions/trapping.r")
write_trapping(m = "all")
write_trapping(m = "ground_only")
