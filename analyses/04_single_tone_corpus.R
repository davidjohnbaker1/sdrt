#======================================================================================================
# Single tone PLUS Corpus Frequency EDA
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
#--------------------------------------------------
# Single Note Analyses ####

# Set Factor For Graphing 
single_table$scale_degree_f <- factor(single_table$scale_degree, 
                                      levels = c("do","ra", "re", "me", "mi",
                                                 "fa", "fi","sol","le", "la", 
                                                 "te", "ti", "do8"))


#--------------------------------------------------
# Add on The Humdrum Frequency Tables 
#--------------------------------------------------

n_gram_counts <- read_csv("experiment_materials/stimuli_lists/cogmir_stimuli.csv")
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

ffh_rt
#ggsave(filename = "ffh_poster/ffh_rt.png",plot = ffh_rt,device = "png")

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

ffh_correct

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
#ggsave(filename = "ffh_poster/ffh_correct.png",plot = ffh_correct,device = "png")
#ggsave(filename = "ffh_poster/ffh_correct_lm.png",plot = ffh_correct_lm,device = "png")
#--------------------------------------------------

