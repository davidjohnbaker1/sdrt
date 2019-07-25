#--------------------------------------------------
# IDyOM Analysis
#--------------------------------------------------
library(tidyverse)

idyom_data <- read.delim("corpus/idyom_data.dat")

idyom_data <- as_tibble(idyom_data)

idyom_data %>%
  select(melody.id, note.id, melody.name, phrase:pulses,
         mode:cpitch.entropy,probability, information.content, entropy) -> idyom_data


View(idyom_data)

# Calculate Scale Degree 
# Find occurances of Patterns in Corpus
# Get IC for those patterns 

#--------------------------------------------------
# Make Scale Degree Calcualtions
# Use Key Sig and Mode 
# Cpitch - referent mod 12 

create_referent <- function(keysig, modal){
  x <- ifelse(test = keysig >= 0, 
              yes = ((keysig * 7) + modal ) %% 12, 
              no = ((keysig * -5) + modal) %% 12  )
  x
}

idyom_data$referent <- create_referent(keysig = idyom_data$keysig, modal = idyom_data$mode)

# Cpint - Referent 
idyom_data$scale_degree <- (idyom_data$cpitch - idyom_data$referent) %% 12
#--------------------------------------------------
View(idyom_data)
source(file = "analyses/find_idyom_gram_functions.R")
#--------------------------------------------------
# Example (245) aka Re, Mi, Fa
gram_finder(245, idyom_data) %>%
  select(melody.name, note.id, scale_degree, information.content, flag) %>%
  group_by(flag) %>% # Group every instance together 
  mutate(sum_ic = sum(information.content)) %>% # cumulative add all notes in pattern
  select(melody.name, sum_ic, flag) %>%
  distinct() %>% 
  ungroup(flag) %>% # 
  summarise(mean_total_ic = mean(sum_ic))
#--------------------------------------------------
# Import in Stimuli List 
multi_table <- read_csv("data/aggregate_data/current_multi_table.csv")


multi_table %>%
  select(stimulus) %>%
  distinct()  -> experimental_stimuli_list 
  
write_csv(x = experimental_stimuli_list, "data/aggregate_data/experimental_stimuli.csv")
helpful_list <- read_csv("data/aggregate_data/experimental_stimuli2.csv")

helpful_list %>%
  print(n = 41)

#======================================================================================================
# Get IDyOM Measures 
#--------------------------------------------------
# please don't judge my code... 

helpful_list %>% print(n = 40)

helpful_list$average_ic <- 0

#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="4 9",]$average_ic <- as.numeric(find_2_grams(4,9, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="4 2",]$average_ic <- as.numeric(find_2_grams(4,2, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="2 10",]$average_ic <- as.numeric(find_2_grams(2, 10, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="7 1",]$average_ic <- as.numeric(find_2_grams(7,1 , idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="4 5",]$average_ic <- as.numeric(find_2_grams(4,5, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="5 4",]$average_ic <- as.numeric(find_2_grams(5,4, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="11 4",]$average_ic <- as.numeric(find_2_grams(11,4, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="0 3",]$average_ic <- as.numeric(find_2_grams(0,3, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="2 0",]$average_ic <- as.numeric(find_2_grams(2,0, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
# START THREE
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="0 9 8",]$average_ic <- as.numeric(find_3_grams(0,9,8, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="1 10 11",]$average_ic <- as.numeric(find_3_grams(1,10,11, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="0 1 2",]$average_ic <- as.numeric(find_3_grams(0,1,2 , idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="9 0 7",]$average_ic <- as.numeric(find_3_grams(9,0,7, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="2 7 4",]$average_ic <- as.numeric(find_3_grams(2,7,4, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="5 4 2",]$average_ic <- as.numeric(find_3_grams(5,4,2 , idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="4 2 0",]$average_ic <- as.numeric(find_3_grams(4,2,0, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="7 5 4",]$average_ic <- as.numeric(find_3_grams(7,5,4, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="0 10 10",]$average_ic <- as.numeric(find_3_grams(0,10,10, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="0 1 2",]$average_ic <- as.numeric(find_3_grams(0,1,2, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="9 0 7",]$average_ic <- as.numeric(find_3_grams(9,0,7, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="2 7 4",]$average_ic <- as.numeric(find_3_grams(2,7,4, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="5 4 2",]$average_ic <- as.numeric(find_3_grams(5,4,2, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="4 2 0",]$average_ic <- as.numeric(find_3_grams(4,2,0, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="7 5 4",]$average_ic <- as.numeric(find_3_grams(7,5,4, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="0 10 10",]$average_ic <- as.numeric(find_3_grams(0,10,10, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
# START FIVE GRAMS 
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="6 4 4 6 4",]$average_ic <- as.numeric(find_5_grams(6,4,4,6,4, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="7 5 4 2 0",]$average_ic <- as.numeric(find_5_grams(7,5,4,2,0, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="7 9 7 5 4",]$average_ic <- as.numeric(find_5_grams(7,9,7,5,4, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="2 2 4 5 2",]$average_ic <- as.numeric(find_5_grams(2,2,4,5,2, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="9 5 4 5 7",]$average_ic <- as.numeric(find_5_grams(9,5,4,5,7, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="9 7 5 4 2",]$average_ic <- as.numeric(find_5_grams(9,7,5,4,2, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="9 8 11 0 7",]$average_ic <- as.numeric(find_5_grams(9,8,11,0,7, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
# START SEVEN 
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="9 8 5 8 4 0 10",]$average_ic <- as.numeric(find_7_grams(9,8,5,8,4,0,10, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="5 7 9 7 5 4 2",]$average_ic <- as.numeric(find_7_grams(5, 7, 9, 7, 5, 4, 2, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="0 11 9 7 5 4 2",]$average_ic <- as.numeric(find_7_grams(0, 11, 9, 7, 5, 4, 2, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="5 8 6 8 5 1 1",]$average_ic <- as.numeric(find_7_grams(5, 8, 6, 8, 5, 1, 1, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="0 4 0 7 4 2 4",]$average_ic <- as.numeric(find_7_grams(0, 4, 0, 7, 4, 2, 4, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation==" 7 9 7 5 4 2 0",]$average_ic <- as.numeric(find_7_grams(7, 9, 7, 5, 4, 2, 0, idyom_data) 
                                                                           %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="0 2 0 11 0 4 2",]$average_ic <- as.numeric(find_7_grams(0, 2, 0, 11, 0, 4, 2, idyom_data) 
                                                                                     %>% get_string_idyom_ic())
#--------------------------------------------------
# START 9 GRAMS
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="11 9 7 9 7 5 7 9 7",]$average_ic <- as.numeric(find_9_grams(11, 9, 7, 9, 7, 5, 7, 9, 7, idyom_data) 
                                                                        %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="4 5 2 11 0 2 4 4 5",]$average_ic <- as.numeric(find_9_grams(4, 5, 2, 11, 0, 2, 4, 4, 5, idyom_data) 
                                                                        %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="7 5 9 11 4 3 2 7 5",]$average_ic <- as.numeric(find_9_grams(7, 5, 9, 11, 4, 3, 2, 7, 5, idyom_data) 
                                                                        %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="2 5 4 2 0 4 3 4 8",]$average_ic <- as.numeric(find_9_grams(2, 5, 4, 2, 0, 4, 3, 4, 8, idyom_data) 
                                                                        %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="7 9 7 5 4 2 0 2 4",]$average_ic <- as.numeric(find_9_grams(7, 9, 7, 5, 4, 2, 0, 2, 4, idyom_data) 
                                                                        %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="2 4 5 7 9 7 5 4 2",]$average_ic <- as.numeric(find_9_grams(2, 4, 5, 7, 9, 7, 5, 4, 2, idyom_data) 
                                                                        %>% get_string_idyom_ic())
#--------------------------------------------------
helpful_list[helpful_list$idyom_notation=="4 5 7 9 7 5 4 2 0",]$average_ic <- as.numeric(find_9_grams(4, 5, 7, 9, 7, 5, 4, 2, 0, idyom_data) 
                                                                        %>% get_string_idyom_ic())

helpful_list %>% print(n= 40)

write_csv(x = helpful_list, "data/aggregate_data/idyom_computation.csv")
#======================================================================================================

