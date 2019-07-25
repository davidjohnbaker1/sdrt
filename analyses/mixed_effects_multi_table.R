#======================================================================================================
# Multi Tone Statistical Modeling
#--------------------------------------------------
# Library ####
library(tidyverse)
library(viridis)
library(scales)
#--------------------------------------------------
# Import Data ####
demographic_data <- read_csv("data/aggregate_data/current_demo_table.csv")
single_table <- read_csv("data/aggregate_data/current_single_table.csv")
multi_table <- read_csv("data/aggregate_data/current_multi_table.csv")
#--------------------------------------------------
# Krumhansl 1982 ratings
krumhansl <- c(6.35,2.23,3.48,2.33,4.38,4.09,2.52,5.19,2.39,3.66,2.29,2.88)
scale_degree <- c("do","ra","re","me","mi","fa","fi","sol","le","la","te","ti")
krummy <- data.frame(krumhansl,scale_degree)
#======================================================================================================
# Set up Tables 
cogmir_counts <- read_csv("experiment_materials/stimuli_lists/cogmir_stimuli.csv")

# Drop X1
multi_table %>%
  select(-X1) -> multi_table

# Clean Up Stimulus Variable For Merge
multi_table$stimulus <- gsub(pattern = "_",replacement = " ", x = multi_table$stimulus)

cogmir_counts %>%
  rename(stimulus = Pattern) -> cogmir_counts

multi_table %>%
  left_join(cogmir_counts) -> SLH_table 

#--------------------------------------------------
# Descriptive Statistics -- Average Scores 

SLH_table %>%
  select(subject, rt, stimulus, score, serial_order) %>%
  group_by(subject) %>%
  mutate(mean_score = mean(score)) %>%
  select(subject, mean_score) %>%
  distinct() %>%
  arrange(-mean_score)

SLH_table %>%
  select(subject, rt, stimulus, score, serial_order, Count) %>%
  group_by(stimulus) %>%
  mutate(mean_score = mean(score), n = n ()) %>%
  select(stimulus, Count, n) %>%
  arrange(stimulus) %>%
  distinct()

#======================================================================================================
# Next Up


fantastic_features <- read_csv("experiment_materials/new_stimuli/MelodyFeatures.csv")

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

#======================================================================================================
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

for_groups <- as.data.frame(names(modeling_table))

for_groups

write_csv(x = for_groups, path = "imgs/group_plots_output.csv")

for_graph_lables <- read_csv("imgs/group_plots_input.csv",
                             col_names = c("names","theory"))
for_graph_lables


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

for_graph_lables %>%
  rename(features = theory) -> for_graph_lables

for_graph_lables %>% print(n = 100)

for_graph_lables %>%
  left_join(feature_plot)

feature_plot %>%
  mutate(features = as.character(features)) %>%
  full_join(for_graph_lables)

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
  theme_minimal() -> fantastic_plot_1 

fantastic_plot_1

#ggsave(filename = "ffh_poster/fantastic_plot_1.png",
#       plot = fantastic_plot_1,
#       device = "png")

modeling_table %>%
  group_by(stimulus) %>%
  mutate(mean_score = mean(score)) %>%
  select(stimulus, mean_score,Gram, quintile) %>%
  distinct() %>%
  arrange(-mean_score) %>%
  ggplot(aes(x = reorder(stimulus, -mean_score), y = mean_score, fill = as.factor(quintile))) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) + 
  labs(title = "Difficulty of Stimuli by Quintle",
       fill = "Quintile",
       x = "Stimuli",
       y = "Mean Score") -> item_difficulty_map

item_difficulty_map

#--------------------------------------------------
# ggsave(filename = "ffh_poster/item_difficulty.png",plot = item_difficulty_map,device = "png")



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