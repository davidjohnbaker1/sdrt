#======================================================================================================
# Multi Tone Analysis 
#--------------------------------------------------
#======================================================================================================
# Exploratory Analysis for CogMir 
#--------------------------------------------------
# Correlation between frequency and accuracy
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
idyom_computations <- read_csv("data/aggregate_data/idyom_computation.csv")
#--------------------------------------------------
# Krumhansl 1982 ratings
krumhansl <- c(6.35,2.23,3.48,2.33,4.38,4.09,2.52,5.19,2.39,3.66,2.29,2.88)
scale_degree <- c("do","ra","re","me","mi","fa","fi","sol","le","la","te","ti")
krummy <- data.frame(krumhansl,scale_degree)
#--------------------------------------------------

#--------------------------------------------------
# Same Analysis, but with Grouped

cogmir_counts <- read_csv("experiment_materials/stimuli_lists/cogmir_stimuli.csv")

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

idyom_computations$stimulus <- gsub(pattern = "_", replacement = " ",x = idyom_computations$stimulus)

SLH_table %>%
  left_join(idyom_computations) -> idyom_analysis_table 


# Set Factor For Graphing 
idyom_analysis_table$Gram <- factor(idyom_analysis_table$Gram, 
                                      levels = c("Two","Three","Five","Seven","Nine"))


idyom_analysis_table %>%
  select(average_ic, score)

cor(idyom_analysis_table$score, idyom_analysis_table$average_ic, use = "complete.obs")

idyom_analysis_table %>%
  group_by(stimulus) %>%
  mutate(avg_score = mean(score)) %>%
  select(stimulus, Count, Gram, quintile, average_ic, avg_score) %>%
  distinct() -> id_regression_table 

id_regression_table %>%
  filter(average_ic != 0) -> id_regression_table

cor(id_regression_table$avg_score, id_regression_table$average_ic, use = "complete.obs")

id_regression_table %>%
  ggplot(aes(x = average_ic, y = avg_score, color = Gram)) +
  geom_point() +
  scale_y_continuous(labels = percent) + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "IDyOM Predicts Average Score via Information Content",
       x = "Average IC of n-gram in MeloSol Corpus",
       y = "Average Score of Sample") -> grouped_idyom_regression_plot


grouped_idyom_regression_plot

id_regression_table %>%
  ggplot(aes(x = average_ic, y = avg_score)) +
  geom_point() +
  scale_y_continuous(labels = percent) + 
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "IDyOM Predicts Average Score via Information Content",
       x = "Average IC of n-gram in MeloSol Corpus",
       y = "Average Score of Sample") -> idyom_regression_plot

idyom_regression_plot


# ggsave(filename = "ffh_poster/grouped_idyom_regression.png",plot = grouped_idyom_regression_plot,device = "png")
id_regression_table %>%
  filter(average_ic  < 20) -> without_longer

wolonger_model <- lm(avg_score ~ average_ic, data = without_longer)
summary(wolonger_model)


model_idyom <- lm(avg_score ~ average_ic, data = id_regression_table)
summary(model_idyom)
plot(model_idyom)

model_number_of_notes <- lm(avg_score ~ Gram, id_regression_table)
summary(model_number_of_notes)
plot(model_number_of_notes)

id_regression_table


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

id_regression_table %>%
  left_join(fantastic_features) -> modeling_table

#======================================================================================================
# Drop 2 grams 

modeling_table %>%
  filter(Gram != "Two") -> modeling_table

modeling_table

#--------------------------------------------------
# More regression

# Krum plots could be next to each other
# Could also show data for each point... so show dispersion bc sd interesting

# Item Difficulty plot 
# Group That 
# Mixed Effects 


#======================================================================================================
# Old Plots 
#--------------------------------------------------

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
