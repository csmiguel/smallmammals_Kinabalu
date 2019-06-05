###.............................................................................
# (c) Miguel Camacho Sánchez
# miguelcamachosanchez@gmail.com // miguelcamachosanchez.weebly.com
# https://scholar.google.co.uk/citations?user=1M02-S4AAAAJ&hl=en
# May 2019
###.............................................................................
#DESCRIPTION: Tables with trapping effort
#PROJECT: https://github.com/csmiguel/smallmammals_Kinabalu
###.............................................................................

library(dplyr)
library(magrittr)
  #Table with trapping sucess
  #variables: Mt, elevation, no. catches, trapnights, trap success

source("src/functions/trapping.r")
write_trapping(m = "all")
write_trapping(m = "ground_only")
