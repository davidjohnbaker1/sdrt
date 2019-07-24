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
# Item Plot to Poster 
# Need to Implement Miller Model and others! 
# Then run fixed effects 
# Cum IC of all the Models 
#--------------------------------------------------
# Import Data 
demographic_data <- read_csv("data/aggregate_data/current_demo_table.csv")
single_table <- read_csv("data/aggregate_data/current_single_table.csv")
multi_table <- read_csv("data/aggregate_data/current_multi_table.csv")
#--------------------------------------------------
# Krumhansl 1982 ratings
krumhansl <- c(6.35,2.23,3.48,2.33,4.38,4.09,2.52,5.19,2.39,3.66,2.29,2.88)
scale_degree <- c("do","ra","re","me","mi","fa","fi","sol","le","la","te","ti")
krummy <- data.frame(krumhansl,scale_degree)

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
       y = "Reaction Time in ms",
       color = "Score",
       shape = "Key") -> sdrt_big

sdrt_big

ggsave(filename = "ffh_poster/sdrt_big.png",plot = sdrt_big,device = "png")

#--------------------------------------------------
# Particpant Main Effects 
single_table %>%
  filter(rt != 9999) %>%
  filter(rt >= 5000) %>%
  group_by(scale_degree,key) %>%
  mutate(rt_means = mean(rt, na.rm = TRUE), 
         mean_correct = mean(score, na.rm = TRUE)) %>%
  filter(scale_degree_f != "NA") %>%
  ggplot(aes(x = scale_degree_f, y = rt, color = as.factor(key), shape = as.factor(score))) +
  geom_point() + 
  theme_minimal() +
  # coord_flip() +
  scale_color_viridis(discrete = TRUE, begin = .20, end = .80) +
  labs(title = "Scale Degree Reaction Time",
       x = "Scale Degree", 
       y = "Reaction Time in ms",
       color = "Key", 
       shape = "Score") +
  facet_wrap(~subject)-> sdrt_big_indv

sdrt_big_indv

ggsave(filename = "ffh_poster/sdrt_big_indv.png",
       plot = sdrt_big_indv,
       device = "png")

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
       y = "Reaction Time in ms",
       color = "Key") -> sdrt_big_key

sdrt_big_key

ggsave(filename = "ffh_poster/sdrt_big_key.png",plot = sdrt_big_key,device = "png")


single_table %>%
  filter(rt != 9999) %>%
  filter(rt >= 5000) %>%
  group_by(scale_degree, key) %>%
  mutate(rt_means = mean(rt, na.rm = TRUE), 
         mean_correct = mean(score, na.rm = TRUE)) %>%
  filter(scale_degree_f != "NA") %>%
  ggplot(aes(x = scale_degree_f, y = mean_correct, color = key)) +
  geom_point() + 
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, begin = .20, end = .80) +
  labs(title = "Scale Degree Reaction Time",
       x = "Scale Degree", 
       y = "Reaction Time in ms", color = "Key") -> sdrt_big_key_correct

sdrt_big_key_correct

ggsave(filename = "ffh_poster/sdrt_big_key_correct.png",
       plot = sdrt_big_key_correct,
       device = "png")

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
  scale_y_continuous(labels = comma) +
  scale_color_viridis(discrete = TRUE, begin = .20, end = .80) +
  labs(title = "Scale Degree Reaction Time",
       x = "Scale Degree", 
       y = "Reaction Time in ms") -> sdrt_big_key_note

sdrt_big_key_note

ggsave(filename = "ffh_poster/sdrt_big_key_note.png",
       plot = sdrt_big_key_note,
       device = "png")

#--------------------------------------------------
# Invert Plot 
single_table %>%
  filter(rt != 9999) %>%
  filter(rt >= 5000) %>%
  group_by(scale_degree) %>%
  mutate(rt_means = mean(rt, na.rm = TRUE), 
         mean_correct = mean(score, na.rm = TRUE)) %>%
  filter(scale_degree_f != "NA") %>%
  mutate(rt_means_invert = 1/rt_means) %>%
  ggplot(aes(x = scale_degree_f, y = rt_means_invert)) +
  geom_point() + 
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  scale_color_viridis(discrete = TRUE, begin = .20, end = .80) +
  labs(title = "Scale Degree Reaction Time (Inverted)",
       x = "Scale Degree", 
       y = "INVERSION") -> sdrt_big_key_note_invert

sdrt_big_key_note_invert

ggsave(filename = "ffh_poster/sdrt_big_key_note.png",
       plot = sdrt_big_key_note,
       device = "png")

single_table %>%
  filter(rt != 9999) %>%
  filter(rt >= 5000) %>%
  group_by(scale_degree) %>%
  mutate(rt_means = mean(rt, na.rm = TRUE), 
         mean_correct = mean(score, na.rm = TRUE)) %>%
  filter(scale_degree_f != "NA") %>%
  ggplot(aes(x = scale_degree_f, y = mean_correct)) +
  geom_point() + 
  scale_y_continuous(label = percent) +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, begin = .20, end = .80) +
  labs(title = "Scale Degree Average Correct",
       x = "Scale Degree", 
       y = "Mean Correct") -> sdrt_big_key_note_correct

sdrt_big_key_note_correct

ggsave(filename = "ffh_poster/sdrt_big_key_note_correct.png",
       plot = sdrt_big_key_note_correct,
       device = "png")
#--------------------------------------------------
# Make Krumhansl Plot 

# Set Factor For Graphing 
krummy$scale_degree_f <- factor(krummy$scale_degree, 
                                      levels = c("do","ra", "re", "me", "mi",
                                                 "fa", "fi","sol","le", "la", 
                                                 "te", "ti", "do8"))

krummy %>%
  ggplot(aes( x = scale_degree_f, y = krumhansl)) +
  geom_point() + 
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, begin = .20, end = .80) +
  labs(title = "Krumhansl Ratings (1982)",
       x = "Scale Degree", 
       y = "Mean Rating") -> krumhansl_1982

krumhansl_1982

ggsave(filename = "ffh_poster/krumhansl_1982.png",
       plot = krumhansl_1982,
       device = "png")


# MULTI PLOT 
cowplot::plot_grid(krumhansl_1982,
                   sdrt_big_key_note, 
                   sdrt_big_key_note_correct,
                   sdrt_big_key_note_invert) -> krum_mutli_plot
  
ggsave(filename = "ffh_poster/krumhansl_multi_plot.png",
       plot = krum_mutli_plot,
       device = "png")



#----------------------------------  

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
  mutate(avg_rt = mean(rt)) %>%
  select(scale_degree_f, avg_correct, avg_rt) %>%
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


#--------------------------------------------------
# Get LE 

single_corz %>%
  left_join(krummy) -> single_corz

cor(single_corz$krumhansl, 
    single_corz$avg_correct,
    use = "pairwise.complete.obs", 
    method = "spearman")

cor(single_corz$krumhansl, 
    single_corz$avg_rt,
    use = "pairwise.complete.obs", 
    method = "spearman")

cor(single_corz$count, 
    single_corz$avg_correct,
    use = "pairwise.complete.obs", 
    method = "spearman")

#--------------------------------------------------
# Fix This one !!! 

# THIS NEEDS ERROR BARS 

single_corz %>%
  ggplot(aes(x = count, y = avg_rt, color = scale_degree_f, label = scale_degree_f)) + 
  geom_point() + 
  geom_text(aes(label=degree), hjust = 1.4, vjust = 1) + 
  theme_minimal() + 
  labs(title = "Correct Responses as Predicted by Distribution in MeloSol Corpus",
       y = "Average Reaction Time in ms",
       x = "Scale Degree Frequency Count") +
  theme(legend.position = "none") +
  scale_x_continuous(label = comma) +
  scale_y_continuous(label = comma) +
  scale_color_viridis(discrete = TRUE, begin = .2, end = .8) -> ffh_rt

ggsave(filename = "ffh_poster/ffh_rt.png",plot = ffh_rt,device = "png")

single_corz %>%
  ggplot(aes(x = count, y = avg_correct, color = scale_degree_f, label = scale_degree_f)) + 
  geom_point() + 
  geom_text(aes(label=degree), hjust = 1.4, vjust = 1) + 
  theme_minimal() + 
  labs(title = "Correct Responses as Predicted by \nDistribution in MeloSol Corpus",
       y = "Average Percent Correct",
       x = "Scale Degree Frequency Count") +
  theme(legend.position = "none") +
  scale_x_continuous(label = comma) +
  scale_y_continuous(label = percent) + 
  scale_color_viridis(discrete = TRUE, begin = .2, end = .8) -> ffh_correct 


single_corz %>%
  ggplot(aes(x = count, y = avg_correct, label = scale_degree_f)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label=degree), hjust = 1.4, vjust = 1) + 
  theme_minimal() + 
  labs(title = "Correct Responses as Predicted by \nDistribution in MeloSol Corpus",
       y = "Average Percent Correct",
       x = "Scale Degree Frequency Count") +
  theme(legend.position = "none") +
  scale_x_continuous(label = comma) +
  scale_y_continuous(label = percent) + 
  scale_color_viridis(discrete = TRUE, begin = .2, end = .8) -> ffh_correct_lm

ffh_correct_lm

ffh_correct

ggsave(filename = "ffh_poster/ffh_correct.png",plot = ffh_correct,device = "png")
ggsave(filename = "ffh_poster/ffh_correct_lm.png",plot = ffh_correct_lm,device = "png")

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
  filter(Gram == "Nine") %>% 
  group_by(stimulus) %>%
  mutate(mean_score = mean(score), log_count = log(Count)) %>%
  ggplot(aes(x = log_count, y = mean_score)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  theme_minimal() + 
  labs(title = "Correct Responses as Predicted by \nDistribution in MeloSol Corpus",
       y = "Average Percent Correct",
       x = "Scale Degree Frequency Count (log scale)",
       subtitle = "9-grams") +
  theme(legend.position = "none") +
  scale_x_continuous(label = comma) +
  scale_y_continuous(label = percent) + 
  scale_color_viridis(discrete = TRUE, begin = .2, end = .8)

#--------------------------------------------------
# Average Scores 

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

# plot all the per stimuli, then overlay the IC !!!!

SLH_table %>% 
  filter(score == 1) %>%
  filter(stimulus == "^3 ^4 ^5  ^6 v5 v4 v3 v2 v1") %>%
  group_by(subject) %>%
  mutate(zRt = scale(rt)) %>%
  ggplot(aes(x = serial_order, y = zRt, color = as.factor(subject))) + 
  geom_point() +
  #geom_line() +
  geom_smooth(method = "auto", se = FALSE) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1,9,1)) +
  scale_fill_viridis(discrete = TRUE) +
  labs(x = "Serial Order",
       y = "Reaction time in MS",
       title = "Single Stim RT",
       subtitle = "^3 ^4 ^5  ^6 v5 v4 v3 v2 v1")

SLH_table %>% 
  filter(score == 1) %>%
  filter(stimulus == "^5 ^6 v5 v4 v3 v2 v1") %>%
  group_by(subject) %>%
  mutate(zRt = scale(rt)) %>%
  ggplot(aes(x = serial_order, y = zRt, color = as.factor(subject))) + 
  geom_point() +
  #geom_line() +
  geom_smooth(method = "auto", se = FALSE) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1,7,1)) +
  scale_fill_viridis(discrete = TRUE) +
  labs(x = "Serial Order",
       y = "Reaction time in MS",
       title = "Single Stim RT",
       subtitle = "^5 ^6 v5 v4 v3 v2 v1")

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

#======================================================================================================
# GARBAGE HERE !
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

ggsave(filename = "ffh_poster/fantastic_plot_1.png",
       plot = fantastic_plot_1,
       device = "png")


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
#======================================================================================================