#======================================================================================================
# Exploratory Analysis for CogMir 
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

#View(cogmir_counts)

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

# View(modeling_table)
#--------------------------------------------------
# Make FANTASTIC Plots , Get Peter Stuff 
library(Hmisc)

modeling_table %>%
  # Only Grab Stimuli where Three or More (for Full FANTASTIC)
  filter(Gram == "Three" | Gram == "Five" | Gram == "Seven" | Gram == "Nine") %>%
  # Now Have all stimuli (28 That Qualify)
  group_by(stimulus) %>%
  # Calculate Average Correct For EacH Stimuli 
  mutate(mean_correct = mean(score)) %>%
  select(mean_correct, Count, mean.entropy:int.cont.glob.dir) %>%
  ungroup(stimulus) %>%
  select(-mode, -h.contour, -stimulus) -> model_cor_table

wolf <- as.data.frame(cor(model_cor_table))

wolf$mean_correct
features <- rownames(wolf)
feature_plot <- as.data.frame(cbind(wolf$mean_correct, features))

feature_plot %>%
  filter(features != "mean_correct") %>%
  mutate(corR = as.numeric(as.character(V1))) %>%
  filter(corR != is.na(NA)) %>%
  mutate(absR = abs(corR)) %>%
  ggplot(aes(x = reorder(features, absR), y = corR)) +
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  labs(title = "Features Correlations with 3,5,7,9 Grams",
       x = "Feature", 
       y = "r") + 
  theme_minimal()

# Add grouping Variable for ENTROPY, FREQUENCY, IDYOM, SENORY 

# Need to drop the weird parts of the sd from humdrum to match above 
# Need Information Contet Via IDyOM
# Need Peter Package 



modeling_table %>%
  # Only Grab Stimuli where Three or More (for Full FANTASTIC)
  filter(Gram == "Three" | Gram == "Five" | Gram == "Seven" | Gram == "Nine") %>%
  # Now Have all stimuli (28 That Qualify)
  group_by(stimulus) %>%
  # Calculate Average Correct For EacH Stimuli 
  mutate(mean_correct = mean(score)) %>%
  # Plot It 
  ggplot(aes(x = len, y = mean_correct)) +
  geom_point() +
  theme_minimal() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Number of Notes", y = "Percentage Correct",
       title = "Number of Notes") +
  scale_y_continuous(labels = percent) -> len_plot 

modeling_table %>%
  # Only Grab Stimuli where Three or More (for Full FANTASTIC)
  filter(Gram == "Three" | Gram == "Five" | Gram == "Seven" | Gram == "Nine") %>%
  # Now Have all stimuli (28 That Qualify)
  group_by(stimulus) %>%
  # Calculate Average Correct For EacH Stimuli 
  mutate(mean_correct = mean(score)) %>%
  # Plot It 
  ggplot(aes(x = step.cont.loc.var, y = mean_correct)) +
  geom_point() +
  theme_minimal() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Local Contour Variations", y = "Percentage Correct",
       title = "Local Contour Variation") +
  scale_y_continuous(labels = percent) -> stepcontourlocalvar_plot 

modeling_table %>%
  # Only Grab Stimuli where Three or More (for Full FANTASTIC)
  filter(Gram == "Three" | Gram == "Five" | Gram == "Seven" | Gram == "Nine") %>%
  # Now Have all stimuli (28 That Qualify)
  group_by(stimulus) %>%
  # Calculate Average Correct For EacH Stimuli 
  mutate(mean_correct = mean(score)) %>%
  # Plot It 
  ggplot(aes(x = Count, y = mean_correct)) +
  geom_point() +
  theme_minimal() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Frequency in MeloSol", y = "Percentage Correct",
       title = "Frequency in MelSol") +
  scale_y_continuous(labels = percent) -> count_plot 

modeling_table %>%
  # Only Grab Stimuli where Three or More (for Full FANTASTIC)
  filter(Gram == "Three" | Gram == "Five" | Gram == "Seven" | Gram == "Nine") %>%
  # Now Have all stimuli (28 That Qualify)
  group_by(stimulus) %>%
  # Calculate Average Correct For EacH Stimuli 
  mutate(mean_correct = mean(score)) %>%
  # Plot It 
  ggplot(aes(x = i.abs.range, y = mean_correct)) +
  geom_point() +
  theme_minimal() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Absolute Interval Range", y = "Percentage Correct",
       title = "Absolute Interval Range") +
  scale_y_continuous(labels = percent) -> intervalabsrange_plot 

modeling_table %>%
  # Only Grab Stimuli where Three or More (for Full FANTASTIC)
  filter(Gram == "Three" | Gram == "Five" | Gram == "Seven" | Gram == "Nine") %>%
  # Now Have all stimuli (28 That Qualify)
  group_by(stimulus) %>%
  # Calculate Average Correct For EacH Stimuli 
  mutate(mean_correct = mean(score)) %>%
  # Plot It 
  ggplot(aes(x = i.entropy, y = mean_correct)) +
  geom_point() +
  theme_minimal() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Interval Entropy", y = "Percentage Correct",
       title = "Interval Entropy") +
  scale_y_continuous(labels = percent) -> intervalentropy_plot 
#--------------------------------------------------
# Univariate Plots 
#--------------------------------------------------
# Number of Notes
len_plot
# Sept Cont Local
stepcontourlocalvar_plot
# i.abs.rang
intervalabsrange_plot
# i.entropy
intervalentropy_plot
# Count 
count_plot
# cum IC 
# Leman
#--------------------------------------------------

cowplot::plot_grid(len_plot,stepcontourlocalvar_plot,intervalabsrange_plot, intervalentropy_plot, count_plot)

#======================================================================================================
# Serial Recall Tasks

multi_table %>%
  filter(trial_length == 9) %>%
  group_by(serial_order) %>%
  mutate(mean_order = mean(score), sd_order = sd(score)) %>%
  select(mean_order, sd_order, serial_order) %>%
  distinct() %>%
  ggplot(aes(x = serial_order, y = mean_order)) +
  geom_point() +
  geom_errorbar(aes(x=serial_order, 
                    y=mean_order, 
                    ymin=mean_order-sd_order, 
                    ymax=mean_order+sd_order)) +
  labs(title = "Serial Order Effects",
       y = "Mean Correct",
       x = "Serial Position")


#======================================================================================================
# TO DO 
# 
# RT Modeling 
# Cor IC with RT 
# Make Sampling Distribution Plot for Talk to show how sampled 
# CLEAN UP YO GOD DAMN CODE 
#======================================================================================================