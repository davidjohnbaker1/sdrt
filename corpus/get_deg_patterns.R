#--------------------------------------------------
# Get Frequent Deg Patterns
#--------------------------------------------------
library(tidyverse)
#--------------------------------------------------

patterns <- read_delim("corpus/deg_patterns.tsv",col_names = c("Count","Pattern","Gram"),delim = "\t")

#--------------------------------------------------
patterns <- as_tibble(patterns)

set.seed(666)

patterns %>%
  group_by(Gram) %>%
  mutate(quintile = ntile(n = 5)) %>%
  group_by(Gram,quintile)  %>%
  sample_n(5) %>%
  arrange(-Count) %>%
  View()



View(pattern_tile)
