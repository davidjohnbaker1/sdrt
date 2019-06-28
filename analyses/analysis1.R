#--------------------------------------------------
# Script Cleaner and Tester Scorer Thing
#--------------------------------------------------
library(tidyverse)
library(ggplot2)
library(viridis)

#--------------------------------------------------
files <- list.files(path = "downloaded_data/")
setwd("downloaded_data/")

big_table <- files %>%
  map(read_csv) %>%
  reduce(rbind)

#pookey <- read_csv("downloaded_data/95676.csv")
# 95676 -- Me test

big_table %>%
  select(rt, trial_type, subject, responses, stimulus, button_pressed,trial_index) -> participant_data

View(participant_data)

participant_data$button_pressed <- recode(participant_data$button_pressed, `0` = "do", `1` = "ra", `2` = "re", `3` = "me", `4` = "mi",
       `5`="fa", `6`= "fi",`7` = "sol", `8` = "le", `9` = "la", `10`="te", `11` = "ti", `12` = "do8")


# Split simtulus into key and sd 

clean_stimuli <- function(x){
  x <- str_remove_all(string = x,pattern = "stimuli/")
  x <- str_remove_all(string = x, pattern = ".mp3")
}

participant_data$stimulus <- clean_stimuli(x = participant_data$stimulus)

participant_data %>% 
  separate(col = stimulus,into =  c("key","sd"),sep =  "-") -> participant_data

participant_data %>%
  mutate(score = if_else(sd == button_pressed, true = 1, 0)) -> participant_data


# Calcualte average RT for scale Degree, only if correct
table(participant_data$button_pressed)

first_trial <- 8
last_trial <- 44

participant_data %>%
  filter(trial_index >= first_trial & trial_index <= last_trial) %>%
  mutate(rt = as.numeric(rt)) %>%
  group_by(sd) %>%
  mutate(mean_rt = mean(rt, na.rm = TRUE), mean_score = mean(score)) 


participant_data$sd_f <- factor(participant_data$sd, levels = c("do","ra", "re", "me", "mi",
                          "fa", "fi","sol","le", "la", 
                          "te", "ti", "do8"))

#--------------------------------------------------
# Data 
#--------------------------------------------------
participant_data %>%
  filter(trial_index >= first_trial & trial_index <= last_trial) %>%
  filter(sd != "null") %>%
  mutate(rt = as.numeric(rt)) %>%
  filter(rt > 5000) %>%
  group_by(sd,key) %>%
  mutate(rt_means = mean(rt, na.rm = TRUE), mean_correct = mean(score, na.rm = TRUE)) %>%
  filter(sd_f != "NA") %>%
  ggplot(aes(x = sd_f, y = rt, color = as.factor(score), shape = key)) +
  geom_point() + 
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, begin = .20, end = .80) +
  labs(title = "Scale Degree Reaction Time",
       x = "Scale Degree", 
       y = "Reaction Time in ms") -> sdrt_big

sdrt_big

#--------------------------------------------------
# Violin 
participant_data %>%
  filter(trial_index >= first_trial & trial_index <= last_trial) %>%
  filter(sd != "null") %>%
  mutate(rt = as.numeric(rt)) %>%
  filter(rt > 5000) %>%
  group_by(sd) %>%
  mutate(rt_means = mean(rt, na.rm = TRUE), mean_correct = mean(score, na.rm = TRUE)) %>%
  filter(sd_f != "NA") %>%
  ggplot(aes(x = sd_f, y = rt)) +
  geom_violin(draw_quantiles = TRUE) + 
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, begin = .20, end = .80) +
  labs(title = "Scale Degree Reaction Time",
       x = "Scale Degree", 
       y = "Reaction Time in ms") -> sdrt_violin

sdrt_violin
#--------------------------------------------------

participant_data %>%
  filter(trial_index >= first_trial & trial_index <= last_trial) %>%
  filter(sd != "null") %>%
  filter(score == 1) %>%
  mutate(rt = as.numeric(rt)) %>%
  group_by(sd) %>%
  mutate(rt_means = mean(rt, na.rm = TRUE), mean_correct = mean(score, na.rm = TRUE)) %>%
  ggplot(aes(x = sd_f, y = rt_means, color = as.factor(score))) +
  geom_point() + 
  theme_minimal() +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Scale Degree Reaction Time, Collapsed", x = "Scale Degree", y = "Reaction Time in ms") -> sdrt_key_big

sdrt_key_big

participant_data %>%
  filter(trial_index >= first_trial & trial_index <= last_trial) %>%
  filter(sd != "null") %>%
  filter(score == 1) %>%
  mutate(rt = as.numeric(rt)) %>%
  group_by(sd, key) %>%
  mutate(rt_means = mean(rt, na.rm = TRUE), mean_correct = mean(score, na.rm = TRUE)) %>%
  ggplot(aes(x = sd_f, y = rt_means, color = as.factor(key))) +
  geom_point() + 
  theme_minimal() +
  scale_color_viridis(discrete = TRUE,begin = .25,end = .75) +
  labs(title = "Scale Degree Reaction Time, Collapsed", x = "Scale Degree", y = "Reaction Time in ms") -> main_effect

main_effect

# Calculate Percent Correct for Scale Degrees


# Next steps, export this, correlate with frequency count data in humdrum corpus 
setwd("..")
humdrum <- read_tsv(file = "melosol_counts.tsv", col_names = c("HumFrequency", "sd"))

humdrum %>%
  filter(sd == "do" | sd == "di" | sd == "re" |
         sd == "me" | sd == "mi"| sd ==  "fa"| sd ==  "fi"| sd ==  "so" | sd ==  "le" | sd == "la"| sd ==  "te"| sd == "ti") -> hum2 

hum2$sd <- recode(hum2$sd, "ra" = "di")

participant_data %>%
  left_join(hum2) -> participant_data


participant_data %>%
  select(-trial_type) %>%
  filter(trial_index >= first_trial & trial_index <= last_trial) %>%
  filter(sd != "null") %>%
  mutate(rt = as.numeric(rt)) %>%
  filter(rt > 5000) %>%
  group_by(sd) %>%
  mutate(rt_means = mean(rt, na.rm = TRUE)) -> cortable

cor(cortable$HumFrequency, cortable$rt_means, use = "complete.obs")

#--------------------------------------------------

exp_n <- length(unique(participant_data$subject))

participant_data %>%
  select(-trial_type) %>%
  filter(trial_index >= first_trial & trial_index <= last_trial) %>%
  filter(sd != "null") %>%
  mutate(rt = as.numeric(rt)) %>%
  filter(rt > 5000) %>%
  group_by(sd,key) %>%
  mutate(rt_means = mean(rt, na.rm = TRUE)) %>%
  mutate(zRT_means = scale(rt_means), zHumFreq = scale(HumFrequency)) %>%
  ggplot(aes(x = sd_f, y = rt, color = as.factor(score), shape = key)) +
  geom_point() + 
  theme_minimal() +
  labs(title = "Scale Degree Reaction Time", subtitle = paste("n = ", exp_n), x = "Scale Degree", y = "Reaction Time in ms")


