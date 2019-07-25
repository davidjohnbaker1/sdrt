#======================================================================================================
# Viz Large Scale Data and Make Plots 
#--------------------------------------------------
# This script makes plots of large scale 
# experiment for visualizing
# Used with FFH SMPC2019 Poster
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
  scale_color_viridis(discrete = TRUE, begin = .20, end = .80) +
  labs(title = "Scale Degree Reaction Time",
       x = "Scale Degree", 
       y = "Reaction Time in ms", color = "Key") -> sdrt_big_key_correct

sdrt_big_key_correct

#ggsave(filename = "ffh_poster/sdrt_big_key_correct.png",
#       plot = sdrt_big_key_correct,
#       device = "png")

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
  scale_color_viridis(discrete = TRUE, begin = .20, end = .80) +
  labs(title = "Scale Degree Reaction Time (Inverted)",
       x = "Scale Degree", 
       y = "INVERSION") -> sdrt_big_key_note_invert

sdrt_big_key_note_invert

#ggsave(filename = "ffh_poster/sdrt_big_key_note.png",
#       plot = sdrt_big_key_note,
#       device = "png")

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

#ggsave(filename = "ffh_poster/sdrt_big_key_note_correct.png",
#       plot = sdrt_big_key_note_correct,
#       device = "png")
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
cowplot::plot_grid(krumhansl_1982,
                   sdrt_big_key_note, 
                   sdrt_big_key_note_correct,
                   sdrt_big_key_note_invert) -> krum_mutli_plot


krum_mutli_plot

#ggsave(filename = "ffh_poster/krumhansl_multi_plot.png",
#       plot = krum_mutli_plot,
#       device = "png")

