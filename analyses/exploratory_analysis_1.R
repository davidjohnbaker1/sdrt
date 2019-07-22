#======================================================================================================
# Exploratory Analysis for CogMir 
#--------------------------------------------------
# Wishlist 
#--------------------------------------------------
# Correlation between frequency and accuracy
#--------------------------------------------------
# Library 
library(tidyverse)
library(viridis)
library(scales)
#--------------------------------------------------
# Import Data 
demographic_data <- read_csv("data/aggregate_data/current_demo_table.csv")
single_table <- read_csv("data/aggregate_data/current_single_table.csv")
multi_table <- read_csv("data/aggregate_data/current_multi_table.csv")
#--------------------------------------------------
# Single Analyses 
names(single_table)
unique(single_table$scale_degree)

# Set Factor For Graphing 
single_table$scale_degree_f <- factor(single_table$scale_degree, 
                                      levels = c("do","ra", "re", "me", "mi",
                                                 "fa", "fi","sol","le", "la", 
                                                  "te", "ti", "do8"))

#--------------------------------------------------
# Data 
#--------------------------------------------------
# All Data 
single_table %>%
  filter(rt != 9999) %>%
  filter(rt >= 5000) %>%
  group_by(scale_degree,key) %>%
  mutate(rt_means = mean(rt, na.rm = TRUE), 
         mean_correct = mean(score, na.rm = TRUE)) %>%
  filter(scale_degree_f != "NA") %>%
  ggplot(aes(x = scale_degree_f, y = rt, color = as.factor(score), shape = key)) +
  geom_point() + 
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, begin = .20, end = .80) +
  labs(title = "Scale Degree Reaction Time",
       x = "Scale Degree", 
       y = "Reaction Time in ms") -> sdrt_big

sdrt_big

#--------------------------------------------------
# Average Over SD and Key 

single_table %>%
  filter(rt != 9999) %>%
  filter(rt >= 5000) %>%
  group_by(scale_degree, key) %>%
  mutate(rt_means = mean(rt, na.rm = TRUE), 
         mean_correct = mean(score, na.rm = TRUE)) %>%
  filter(scale_degree_f != "NA") %>%
  ggplot(aes(x = scale_degree_f, y = rt_means, color = key)) +
  geom_point() + 
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, begin = .20, end = .80) +
  labs(title = "Scale Degree Reaction Time",
       x = "Scale Degree", 
       y = "Reaction Time in ms") -> sdrt_big_key

sdrt_big_key

#--------------------------------------------------
# Average Over Keys 

single_table %>%
  filter(rt != 9999) %>%
  filter(rt >= 5000) %>%
  group_by(scale_degree) %>%
  mutate(rt_means = mean(rt, na.rm = TRUE), 
         mean_correct = mean(score, na.rm = TRUE)) %>%
  filter(scale_degree_f != "NA") %>%
  ggplot(aes(x = scale_degree_f, y = rt_means)) +
  geom_point() + 
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, begin = .20, end = .80) +
  labs(title = "Scale Degree Reaction Time",
       x = "Scale Degree", 
       y = "Reaction Time in ms") -> sdrt_big_key_note

sdrt_big_key_note

#--------------------------------------------------
# Add on The Humdrum Frequency Tables 
#--------------------------------------------------

n_gram_counts <- read_csv("stimuli_lists/cogmir_stimuli.csv")
single_n_counts <- read_tsv("corpus/single_data.tsv", col_names = c("count","degree","gram","scale_degree"))

single_table %>%
  select(-stimuli_together,-X1) %>%
  filter(rt != 9999) %>%
  group_by(scale_degree) %>%
  mutate(avg_correct = mean(score)) %>%
  select(scale_degree_f, avg_correct) %>%
  distinct() %>%
  arrange(-avg_correct) -> single_1 

single_1 %>%
  left_join(single_n_counts) -> single_corz 

single_corz %>%
  filter(degree != "1-") %>%
  filter(degree != "7+") %>%
  filter(degree != "3+") %>%
  filter(degree != "6-") %>%
  filter(degree != "3-") -> single_corz

cor(single_corz$count, single_corz$avg_correct,use = "pairwise.complete.obs")

single_corz %>%
  ggplot(aes(x = count, y = avg_correct, color = scale_degree_f, label = scale_degree_f)) + 
  geom_point() + 
  geom_text(aes(label=degree), hjust = 1.4, vjust = 1) + 
  theme_minimal() + 
  labs(title = "Correct Responses as Predicted by Distribution in MeloSol Corpus",
       y = "Average Percent Correct",
       x = "Scale Degree Frequency Count") +
  theme(legend.position = "none") +
  scale_x_continuous(label = comma) +
  scale_y_continuous(label = percent) + 
  scale_color_viridis(discrete = TRUE, begin = .2, end = .8) 
#--------------------------------------------------
# Same Analysis, but with Grouped

cogmir_counts <- read_csv("stimuli_lists/cogmir_stimuli.csv")
View(cogmir_counts)

cogmir_counts$Pattern

# Drop X1
multi_table %>%
  select(-X1) -> multi_table

# Clean Up Stimulus Variable For Merge
multi_table$stimulus <- gsub(pattern = "_",replacement = " ", x = multi_table$stimulus)

cogmir_counts %>%
  rename(stimulus = Pattern) -> cogmir_counts

multi_table %>%
  left_join(cogmir_counts) -> SLH_table 

SLH_table %>% 
  filter(Gram == "Three") %>%
  group_by(stimulus) %>%
  mutate(mean_score = mean(score)) %>%
  ggplot(aes(x = Count, y = mean_score, color = as.factor(quintile))) +
  geom_point() +
  theme_minimal() + 
  labs(title = "Correct Responses as Predicted by Distribution in MeloSol Corpus",
       y = "Average Percent Correct",
       x = "Scale Degree Frequency Count") +
  theme(legend.position = "none") +
  scale_x_continuous(label = comma) +
  scale_y_continuous(label = percent) + 
  scale_color_viridis(discrete = TRUE, begin = .2, end = .8) 
#======================================================================================================
# Next Up


fantastic_features <- read_csv("new_stimuli/MelodyFeatures.csv")

# Need to get rest of FANASTIC Materials 

fantastic_features %>%
  rename(stimulus = file.id) -> fantastic_features

fantastic_features$stimulus <- gsub(pattern = "m",replacement = "", fantastic_features$stimulus)
fantastic_features$stimulus <- gsub(pattern = "ono",replacement = "", fantastic_features$stimulus)
fantastic_features$stimulus <- gsub(pattern = "_",replacement = " ", fantastic_features$stimulus)
fantastic_features$stimulus <- str_trim(fantastic_features$stimulus, side = "right")

fantastic_features$stimulus

SLH_table %>%
  left_join(fantastic_features) -> modeling_table

#--------------------------------------------------
# Mixed Effects Modeling
library(lmerTest)
library(MuMIn)



# This needs to be binomial OR predict average scores 
toy_model <- lmer(score ~ Count + (1 | subject), data = modeling_table)
summary(toy_model)

r.squaredGLMM(toy_model)

View(modeling_table)

#======================================================================================================
# Serial Recall Tasks
# Correlation Plots of Different Features 


# Make Sampling Distribution Plot for Talk to show how sampled 


