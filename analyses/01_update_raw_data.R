#======================================================================================================
# Update Current Data
#--------------------------------------------------
library(tidyverse)
options(readr.num_columns = 0)
#--------------------------------------------------
setwd("data/")
source("../analyses/cleaning/clean_sdrt_long.R")
create_demo_table()
create_single_data()
create_multi_data()

#--------------------------------------------------
setwd("demo/")
bind_demo_table() 
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
