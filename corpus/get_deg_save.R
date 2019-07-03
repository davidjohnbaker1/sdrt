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
  sample_n(3) %>%
  arrange(-Count) %>%
  View()

patterns %>%
  group_by(Gram) %>%
  mutate(quintile = ntile(n = 5)) %>%
  group_by(Gram,quintile)  %>%
  slice(1:5) %>%
  arrange(-Count) -> cogmir_stimuli

write_csv(x = cogmir_stimuli,path = "cogmir_stimuli.csv")

patterns %>%
  group_by(Gram) %>%
  mutate(quintile = ntile(n = 5)) %>%
  group_by(Gram,quintile)  %>%
  slice(1) %>%
  arrange(-Count) -> cogmir_stimuli_short

write_csv(x = cogmir_stimuli_short,path = "cogmir_stimuli_short.csv")

#--------------------------------------------------
# Export Current List of Degs
# Make them humdrum spines 
# Conver them to midi
# Add on Piano Cadence
# Export List to Stimuli directory
# Add named info to jsPsych
#--------------------------------------------------
# Write R Script for Scoring 
#--------------------------------------------------
