#======================================================================================================
# Extract Demographic and Response Data for
# long_sdrt.html 
# David John Baker 

# MAKE ME A LOOPING FUNCTION!!!!

#--------------------------------------------------
# Read in Libraries 
library(tidyverse)

#--------------------------------------------------
# Import Data 
drafter <- read_csv("001.csv")
View(drafter)
#======================================================================================================
# Get Survey Questions
#--------------------------------------------------
# Pre 
subject_id <- drafter$subject[1]
age <- drafter$responses[2]
education <- drafter$responses[3]
solfege <- drafter$responses[4]
AP <- drafter$responses[5]
weeks_taking <- drafter$responses[6]
years_teaching <- drafter$responses[7]
#--------------------------------------------------
# Post 
strategies <- drafter$responses[297]
thoughts <- drafter$responses[298]
piano <- drafter$responses[299]
#--------------------------------------------------
# Create Demographic Table 

demo_table <- cbind(subject_id, age, education, AP, weeks_taking, years_teaching,
                    stragegies, thoughts, piano)


#--------------------------------------------------
# Write Table Here !!!!!!!!!!!!!!!!!!

demo_table

#======================================================================================================
# Create Data for Single Trial 
#--------------------------------------------------
# Index Out Just Single Trial Rows 
single_trial <- drafter %>% filter(trial_index >= 11 & trial_index <= 51) 

#--------------------------------------------------
# Fix Single Trial SD 

# Clean up Stimuli Identifiers 
single_trial %>%
  mutate(stimuli_together = str_remove_all(stimulus, pattern = "stimuli/long/")) %>%
  mutate(drop_mp3 = str_remove_all(stimuli_together, pattern = "\\.mp3")) %>%
  separate(col = drop_mp3, into = c("key","scale_degree")) -> single_trial

# Recode Button Pressed for scoring 
single_trial$button_pressed <- recode(single_trial$button_pressed, `0` = "do", `1` = "ra", `2` = "re", `3` = "me", `4` = "mi",
                                          `5`="fa", `6`= "fi",`7` = "sol", `8` = "le", `9` = "la", `10`="te", `11` = "ti", `12` = "do")

# Score Trial 
single_trial %>%
  mutate(score = if_else(scale_degree == button_pressed, true = 1, 0)) -> single_trial

# Bind 
single_trial %>%
  select(subject, rt, button_pressed, key, scale_degree, score, stimuli_together) -> single_trial

#--------------------------------------------------
# This is where you Write this one 

single_trial

#======================================================================================================
# Do Multi Trial 
#--------------------------------------------------
# Add Trial number of notes blocking with fill 
multi_trial <- drafter %>%
  filter(trial_index >= 52)

#--------------------------------------------------
# Create Table that has just what subject pressed and rt associated with trial 
multi_trial %>%
  select(trial_type, stimulus, button_pressed,rt) %>%
  mutate(file_names_1 = str_replace_all(stimulus,"new_stimuli/mp3/","")) %>%
  mutate(file_name = str_remove_all(file_names_1, ".mp3")) %>%
  select(trial_type, file_name, button_pressed,rt) %>%
  mutate(file_name_group = str_remove_all(file_name, "<p>.*$")) %>%
  mutate(file_name_group = na_if(file_name_group,"")) %>%
  fill(file_name_group,.direction = "down") %>%
  select(trial_type, file_name_group, button_pressed,rt) %>%
  mutate(file_name_group = str_remove_all(file_name_group, "melody_")) %>%
  mutate(file_name_group = str_remove_all(file_name_group, "_$")) %>%
  mutate(file_name_group = str_remove_all(file_name_group, "_buffer")) %>%
  filter(trial_type == "html-button-response") %>%
  select(button_pressed,rt) %>%
  mutate(button_pressed_vector = button_pressed) %>%
  select(button_pressed_vector, rt) -> button_pressed_vector

#--------------------------------------------------
# Create table where stimuli is separated and has grouping
# variable to merge with just button and RT 
multi_trial %>%
  select(trial_type, stimulus, button_pressed) %>%
  mutate(file_names_1 = str_replace_all(stimulus,"new_stimuli/mp3/","")) %>%
  mutate(file_name = str_remove_all(file_names_1, ".mp3")) %>%
  select(trial_type, file_name, button_pressed) %>%
  mutate(file_name_group = str_remove_all(file_name, "<p>.*$")) %>%
  mutate(file_name_group = na_if(file_name_group,"")) %>%
  fill(file_name_group,.direction = "down") %>%
  select(trial_type, file_name_group, button_pressed) %>%
  mutate(file_name_group = str_remove_all(file_name_group, "melody_")) %>%
  mutate(file_name_group = str_remove_all(file_name_group, "_$")) %>%
  mutate(file_name_group = str_remove_all(file_name_group, "_buffer")) %>%
  filter(trial_type == "audio-keyboard-response") %>%
  mutate(trial_response = strsplit(file_name_group,"_")) %>% # MONEY!!!!
  unnest(trial_response) %>%
  mutate(trial_response = str_remove_all(trial_response, "v")) %>%
  mutate(trial_response = str_remove_all(trial_response, "\\^")) %>%
  mutate(trial_answer = recode(trial_response, 
                                 "1" = "do", 
                                 "1+" = "ra",
                                 "2-" = "ra",
                                 "2" = "re",
                                 "2+" ="me", # ENHARMONIC ENCODING 
                                 "3-" = "me", 
                                 "3" = "mi",
                                 "4"="fa", 
                                 "4+" = "fi",
                                 "5-" ="fi", # Enharmonic Encoding 
                                 "5" = "sol", 
                                 "5+" = "le", # Enharmonic Encoding 
                                 "6-" = "le", 
                                 "6" = "la", 
                                 "6+" = "te", # Enharmonic encoding 
                                 "7-" ="te", 
                                 "7" = "ti", 
                                 "1" = "do")) -> gooba 

#--------------------------------------------------
# Join the Two together 
new_data <- cbind(gooba, button_pressed_vector) 

#--------------------------------------------------
# Drop that extra blank column that got in there 
new_data %>%
  select(1:2,4:7) -> new_data

#--------------------------------------------------
# Recode for button pressed 
new_data %>%
  mutate(button_pressed_vector_c = recode(button_pressed_vector, 
                                          "0" = "do", 
                                          "1" = "ra", 
                                          "2" = "re", 
                                          "3" = "me", 
                                          "4" = "mi",
                                          "5" = "fa", 
                                          "6" = "fi",
                                          "7" = "sol", 
                                          "8" = "le", 
                                          "9" = "la", 
                                          "10" = "te", 
                                          "11" = "ti", 
                                          "12" = "do")) -> new_data

#--------------------------------------------------
# Score Trial 
new_data %>%
  mutate(score = if_else(trial_answer == button_pressed_vector_c, true = 1, 0)) -> new_data

#--------------------------------------------------
# Add length variable for later scoring  
new_data %>%
  mutate(trial_length = str_count(new_data$file_name_group,pattern = "_") + 1) -> new_data

#--------------------------------------------------
# Add identifying subject number 
new_data %>%
  mutate(subject = drafter$subject[1]) -> new_data

#--------------------------------------------------
# Clean Up to Match Single Variable 

new_data %>%
  select(subject, file_name_group, rt, button_pressed_vector_c, trial_answer, score, trial_length) %>%
  mutate(stimulus = file_name_group) %>%
  select(-file_name_group) -> multi_response_data 

multi_response_data %>%
  group_by(stimulus) %>%
  mutate(serial_order = row_number()) %>%
  ungroup(stimulus) -> multi_response_data
#--------------------------------------------------
# HERE WRITE TABLE 

multi_response_data 

#======================================================================================================
