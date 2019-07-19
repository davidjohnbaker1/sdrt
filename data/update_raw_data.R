#======================================================================================================
# Update Current Data
#--------------------------------------------------
library(tidyverse)
options(readr.num_columns = 0)
#--------------------------------------------------
setwd("data/")
source("../analyses/clean_sdrt_long.R")
create_demo_table()
create_single_data() # WARNING HERE 
create_multi_data()

#--------------------------------------------------
setwd("demo/")
bind_demo_table() # Warning here 
junk <- dir(pattern="_data") 
file.remove(junk)
setwd("..")

setwd("single/")
bind_single_table()
junk <- dir(pattern="_data") 
file.remove(junk)
setwd("..")

setwd("multi/")
bind_multi_table()
junk <- dir(pattern="_data") 
file.remove(junk)
setwd("../..")
#--------------------------------------------------