#======================================================================================================
# Viz Large Scale Data and Make Plots 
#--------------------------------------------------
# This script makes plots of large scale 
# experiment for visualizing
# Used with FFH SMPC2019 Poster
library(scales)
library(viridis)
library(tidyverse)
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
#--------------------------------------------------
# Single Note Analyses ####

# Set Factor For Graphing 
single_table$scale_degree_f <- factor(single_table$scale_degree, 
                                      levels = c("do","ra", "re", "me", "mi",
                                                 "fa", "fi","sol","le", "la", 
                                                 "te", "ti", "do8"))



#--------------------------------------------------
# Visualize Single Data #### 
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

#--------------------------------------------------
# ggsave(filename = "ffh_poster/sdrt_big.png",plot = sdrt_big,device = "png")

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

#ggsave(filename = "ffh_poster/sdrt_big_indv.png",
#       plot = sdrt_big_indv,
#       device = "png")

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

#ggsave(filename = "ffh_poster/sdrt_big_key.png",plot = sdrt_big_key,device = "png")


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
  scale_y_continuous(label = percent) +
  scale_color_viridis(discrete = TRUE, begin = .20, end = .80) +
  labs(title = "Scale Degree Reaction Time",
       x = "Scale Degree", 
       y = "Percent Correct", color = "Key") -> sdrt_big_key_correct

sdrt_big_key_correct

#ggsave(filename = "ffh_poster/sdrt_big_key_correct.png",
#       plot = sdrt_big_key_correct,
#       device = "png")

#--------------------------------------------------
# Average Over Keys

std_e_m <- function(x) sd(x)/sqrt(length(x))

single_table %>%
  filter(rt != 9999) %>%
  filter(rt >= 5000) %>%
  group_by(scale_degree) %>%
  mutate(rt_means = mean(rt, na.rm = TRUE),
         rt_sd = std_e_m(rt),
         mean_correct = mean(score, na.rm = TRUE)) %>%
  filter(scale_degree_f != "NA") %>%
  ggplot(aes(x = scale_degree_f, y = rt_means)) +
  geom_errorbar(aes(ymin=rt_means-rt_sd, ymax=rt_means+rt_sd), width=.2,
                position=position_dodge(.9)) +
  geom_point() + 
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  scale_color_viridis(discrete = TRUE, begin = .20, end = .80) +
  labs(title = "Scale Degree Reaction Time", 
       x = "Scale Degree", 
       y = "Reaction Time in ms") -> sdrt_big_key_note

sdrt_big_key_note

#ggsave(filename = "ffh_poster/sdrt_big_key_note.png",
#       plot = sdrt_big_key_note,
#       device = "png")

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
  # theme(axis.title.y = element_blank(), axis.text.y = element_blank()) +
  scale_color_viridis(discrete = TRUE, begin = .20, end = .80) +
  labs(title = "Inverted Scale Degree Reaction Time",
       x = "Scale Degree", 
       y = "INVERSION") -> sdrt_big_key_note_invert

sdrt_big_key_note_invert

#ggsave(filename = "ffh_poster/sdrt_big_key_note_invert.png",
#       plot = sdrt_big_key_note_invert,
#       device = "png")

single_table %>%
  filter(rt != 9999) %>%
  filter(rt >= 5000) %>%
  group_by(scale_degree) %>%
  mutate(rt_means = mean(rt, na.rm = TRUE), 
         mean_correct = mean(score, na.rm = TRUE),
         correct_sem = std_e_m(score)) %>%
  filter(scale_degree_f != "NA") %>%
  ggplot(aes(x = scale_degree_f, y = mean_correct)) +
  geom_errorbar(aes(ymin=mean_correct-correct_sem, ymax=mean_correct+correct_sem), width=.2,
                position=position_dodge(.9)) +
  geom_point() + 
  scale_y_continuous(label = percent) +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, begin = .20, end = .80) +
  labs(title = "Scale Degree Average Correct",
       x = "Scale Degree", 
       y = "Mean Correct") -> sdrt_big_key_note_correct

sdrt_big_key_note_correct

#ggsave(filename = "ffh_poster/sdrt_big_key_note_correct.png",
#       plot = sdrt_big_key_note_correct,
#       device = "png")

#--------------------------------------------------
# What if it's FIRST pitch of melody? (Huron, 2006, p. 65, 66)
#--------------------------------------------------

huron_start_table <- read_csv("data/aggregate_data/huron_start_table.csv")

huron_start_table %>%
  rename(scale_degree_pc = scale_degree,
         melody_start_counts = counts) -> huron_start_table

huron_start_table$scale_degree <- c("do","ra", "re", "me", "mi",
                                    "fa", "fi","sol","le", "la", 
                                    "te", "ti")

huron_start_table$scale_degree_f <- factor(huron_start_table$scale_degree, 
                                           levels = c("do","ra", "re", "me", "mi",
                                                      "fa", "fi","sol","le", "la", 
                                                      "te", "ti"))

huron_start_table %>%
  ggplot(aes( x = scale_degree_f,y = melody_start_counts)) +
  geom_point(stat = 'identity') +
  theme_minimal() +
  labs(title = "Starting Scale Degree of Melody",
       x = "Scale Degree", 
       y = "Frequency") -> huron_start_table_plot

huron_start_table

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

#ggsave(filename = "ffh_poster/krumhansl_1982.png",
#       plot = krumhansl_1982,
#       device = "png")


# MULTI PLOT 
corpus_counts <- read_csv("data/aggregate_data//for_krum_multi_plot.csv")

# Set Factor For Graphing 
corpus_counts$scale_degree_f <- factor(corpus_counts$scale_degree, 
                                      levels = c("do","ra", "re", "me", "mi",
                                                 "fa", "fi","sol","le", "la", 
                                                 "te", "ti"))

corpus_counts %>%
  filter(degree != "1-") %>% # Enharmonic 
  filter(degree != "6-") %>% # Remove Minor Notes (Since Major Experiment Prime)
  filter(degree != "6+") %>% # Remove Minor Notes (Since Major Experiment Prime)
  filter(degree != "7+") %>% 
  filter(degree != "5-") %>% 
  filter(degree != "3+") -> corpus_counts

corpus_counts %>%
  ggplot(aes(x = scale_degree_f, y = count)) + 
  geom_point() + 
  scale_y_continuous(label = comma) +
  theme_minimal() +
  labs(title = "Frequency Count in MeloSol",
       x = "Scale Degree", 
       y = "Frequency") -> melo_sol_counts

cowplot::plot_grid(sdrt_big_key_note, 
                   sdrt_big_key_note_invert,
                   sdrt_big_key_note_correct,
                   melo_sol_counts,krumhansl_1982,
                   huron_start_table_plot, 
                   ncol = 1, 
                   nrow = 6) -> krum_mutli_plot


krum_mutli_plot

ggsave(filename = "ffh_poster/krumhansl_multi_plot.png",
       plot = krum_mutli_plot,
       device = "png")

library(ggcorrplot)


corpus_counts %>%
  left_join(krummy) %>%
  left_join(huron_start_table) %>%
  select(avg_correct, avg_rt, count, krumhansl, melody_start_counts) %>%
  round(2) %>%
  rename(`Average Correct` = avg_correct,
         `Average Reaction Time` = avg_rt,
         `Frequency in MeloSol` = count,
         `Krumhansl 1982` = krumhansl,
         `Melody Start Counts` = melody_start_counts) %>%
  cor(method = "spearman") %>%
  xtable::xtable()

corpus_counts %>%
  left_join(krummy) %>%
  select(avg_correct, avg_rt, count, krumhansl) %>%
  round(2) %>%
  rename(`Average Correct` = avg_correct,
         `Average Reaction Time` = avg_rt,
         `Frequency in MeloSol` = count,
         `Krumhansl 1982` = krumhansl) %>%
  cor(method = "spearman") %>%
  ggcorrplot(hc.order = TRUE, type = "lower",
             lab = TRUE,colors = c("#3b1c8c","#21908d","#5ac865"),
             tl.cex = 20,
             pch.cex = 20,
             digits = 3) -> cor_heat_map

cor_heat_map
  
ggsave(filename = "ffh_poster/cor_heat_map.png",
       plot = cor_heat_map,
       device = "png")
