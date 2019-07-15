#--------------------------------------------------
# Functions to Score SDRT Long
#--------------------------------------------------
# * Takes Raw Scores and Creates 3 Tables
# * Demographic Table
# * Single Item Table
# * Multi-Item Table 
#--------------------------------------------------
library(tidyverse)
# Create Demographic Tables 

create_demo_table <- function(fns=list.files(pattern=".csv")){
  for(i in seq(along=fns)){

    experiment_data <- read_csv(fns[i])
    
    print(paste("Now Working On File",fns[i]))

    # Get Survey Questions
    #--------------------------------------------------
    # Pre 
    subject_id <- experiment_data$subject[1]
    age <- experiment_data$responses[2]
    education <- experiment_data$responses[3]
    solfege <- experiment_data$responses[4]
    AP <- experiment_data$responses[5]
    weeks_taking <- experiment_data$responses[6]
    years_teaching <- experiment_data$responses[7]
    #--------------------------------------------------
    # Post 
    strategies <- experiment_data$responses[297]
    thoughts <- experiment_data$responses[298]
    piano <- experiment_data$responses[299]
    #--------------------------------------------------
    # Create Demographic Table 
    
    demo_table <- cbind(subject_id, age, education, AP, weeks_taking, years_teaching,
                        strategies, thoughts, piano)
    
    write.table(demo_table,paste0("demo/",substr(fns[i],1,nchar(fns[i])-4),"_demo_data.csv"),sep=",",col.names=TRUE,row.names=FALSE)
    
    #--------------------------------------------------  
  }
}

create_single_data <- function(fns=list.files(pattern=".csv")){
  
  for(i in seq(along=fns)){
    
    experiment_data <- read_csv(fns[i])
    
    print(paste("Now Working On File",fns[i]))
    
    #======================================================================================================
    # Create Data for Single Trial 
    #--------------------------------------------------
    # Index Out Just Single Trial Rows 
    single_trial <- experiment_data %>% filter(trial_index >= 11 & trial_index <= 51) 
    
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
    
    write.table(single_trial,paste0("single/",substr(fns[i],1,nchar(fns[i])-4),"_single_data.csv"),sep=",",col.names=TRUE,row.names=FALSE)
    
  
    }
  
}

create_multi_data <- function(fns=list.files(pattern=".csv")){
  
  for(i in seq(along=fns)){
    
  experiment_data <- read_csv(fns[i])
  
  print(paste("Now Working On File",fns[i]))
  
  
  #======================================================================================================
  # Do Multi Trial 
  #--------------------------------------------------
  # Add Trial number of notes blocking with fill 
  multi_trial <- experiment_data %>% filter(trial_index >= 52)
  
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
  
  write.table(multi_response_data,paste0("multi/",substr(fns[i],1,nchar(fns[i])-4),"_multi_data.csv"),sep=",",col.names=TRUE,row.names=FALSE)
  
  } 
  
}