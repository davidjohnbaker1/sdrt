#======================================================================================================
# Multi Tone Analysis 
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
cogmir_counts <- read_csv("experiment_materials/stimuli_lists/cogmir_stimuli.csv")
fantastic_features <- read_csv("experiment_materials/new_stimuli/MelodyFeatures.csv")
#======================================================================================================
# Clean FANTASTIC 
#--------------------------------------------------
fantastic_features %>%
  rename(stimulus = file.id) -> fantastic_features

fantastic_features$stimulus <- gsub(pattern = "m",replacement = "", fantastic_features$stimulus)
fantastic_features$stimulus <- gsub(pattern = "ono",replacement = "", fantastic_features$stimulus)
fantastic_features$stimulus <- gsub(pattern = "_",replacement = " ", fantastic_features$stimulus)
fantastic_features$stimulus <- str_trim(fantastic_features$stimulus, side = "right")

fantastic_features

#--------------------------------------------------
# Join Perceptal Data to CogMIR counts

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

idyom_analysis_table

#======================================================================================================
idyom_analysis_table %>%
  left_join(fantastic_features) -> modeling_table

for_groups <- as.data.frame(names(modeling_table))

for_groups

#write_csv(x = for_groups, path = "imgs/group_plots_output.csv")

#--------------------------------------------------
# Names for Table

for_graph_lables <- read_csv("pilot_imgs/group_plots_input.csv",
                             col_names = c("names","theory"))
for_graph_lables




#======================================================================================================
# Make Plots for Poster 

modeling_table %>%
  group_by(stimulus) %>%
  mutate(mean_score = mean(score),
         score_sem = std_e_m(score)) %>%
  select(stimulus, mean_score,Gram, quintile, score_sem) %>%
  distinct() %>%
  arrange(-mean_score) %>%
  ggplot(aes(x = reorder(stimulus, -mean_score), y = mean_score, fill = as.factor(quintile))) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin=mean_score-score_sem, ymax=mean_score+score_sem), width=.2,
                position=position_dodge(.9)) +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(label = percent) +
  scale_fill_viridis(discrete = TRUE) + 
  labs(title = "Difficulty of Stimuli by Quintile",
       fill = "Quintile",
       x = "Stimuli",
       y = "Mean Score") -> item_difficulty_map

 item_difficulty_map

#ggsave(filename = "ffh_poster/item_map.png",plot = item_difficulty_map, device = "png")



#======================================================================================================
# Make Univariate Feature Cow-Plot
#--------------------------------------------------

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
# extras 
#--------------------------------------------------
# tonalness, 
modeling_table %>%
  # Only Grab Stimuli where Three or More (for Full FANTASTIC)
  filter(Gram == "Three" | Gram == "Five" | Gram == "Seven" | Gram == "Nine") %>%
  # Now Have all stimuli (28 That Qualify)
  group_by(stimulus) %>%
  # Calculate Average Correct For EacH Stimuli 
  mutate(mean_correct = mean(score)) %>%
  # Plot It 
  ggplot(aes(x = tonalness, y = mean_correct)) +
  geom_point() +
  theme_minimal() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Tonalness", y = "Percentage Correct",
       title = "Tonalness")  -> tonalness_plot 


#p.entropy
modeling_table %>%
  # Only Grab Stimuli where Three or More (for Full FANTASTIC)
  filter(Gram == "Three" | Gram == "Five" | Gram == "Seven" | Gram == "Nine") %>%
  # Now Have all stimuli (28 That Qualify)
  group_by(stimulus) %>%
  # Calculate Average Correct For EacH Stimuli 
  mutate(mean_correct = mean(score)) %>%
  # Plot It 
  ggplot(aes(x = p.entropy, y = mean_correct)) +
  geom_point() +
  theme_minimal() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Pitch Entropy", y = "Percentage Correct",
       title = "Pitch Entropy") -> pitchentropy_plot 

# int.cont.glob.dir
modeling_table %>%
  # Only Grab Stimuli where Three or More (for Full FANTASTIC)
  filter(Gram == "Three" | Gram == "Five" | Gram == "Seven" | Gram == "Nine") %>%
  # Now Have all stimuli (28 That Qualify)
  group_by(stimulus) %>%
  # Calculate Average Correct For EacH Stimuli 
  mutate(mean_correct = mean(score)) %>%
  # Plot It 
  ggplot(aes(x = int.cont.glob.dir, y = mean_correct)) +
  geom_point() +
  theme_minimal() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "int.cont.glob.dir", y = "Percentage Correct",
       title = "Interpolation Contour Global Direction") -> intcontglobdir_plot 


#======================================================================================================
# Make IDYOM Univariate Plot 
#--------------------------------------------------
idyom_analysis_table$Gram <- factor(idyom_analysis_table$Gram, 
                                    levels = c("Two","Three","Five","Seven","Nine"))


idyom_analysis_table %>%
  group_by(stimulus) %>%
  mutate(avg_correct = mean(score)) %>%
  select(average_ic, avg_correct) %>%
  distinct() -> goofy 

# Low Correlation 
cor(goofy$average_ic, goofy$avg_correct, use = "complete.obs")

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
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "IDyOM: Average Information Content",
       x = "Average IC of n-gram in MeloSol Corpus",
       y = "Average Score of Sample") -> idyom_regression_plot

idyom_regression_plot

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
idyom_regression_plot
# Tonalenss 
tonalness_plot

# pitch entropy 
pitchentropy_plot
# int 
intcontglobdir_plot
# Leman
# MAKE MEEE
#--------------------------------------------------

cowplot::plot_grid(len_plot,stepcontourlocalvar_plot,intervalabsrange_plot, 
                   intervalentropy_plot, count_plot, idyom_regression_plot,
                   tonalness_plot, pitchentropy_plot, intcontglobdir_plot) -> cow2

cow2

#ggsave(filename = "ffh_poster/ffh_cow2.png",plot = cow2, device = "png")

#======================================================================================================
# Make FANTASTIC 
#--------------------------------------------------
# 3, 5, 7 , 9 Univariate Features

modeling_table %>%
  # Only Grab Stimuli where Three or More (for Full FANTASTIC)
  filter(Gram == "Three" | Gram == "Five" | Gram == "Seven" | Gram == "Nine") %>%
  group_by(stimulus) %>%
  mutate(avg_correct = mean(score)) %>%
  select(avg_correct, Count, mean.entropy:int.cont.glob.dir, average_ic) %>%
  distinct() %>%
  ungroup(stimulus) %>%
  select(-mode, -h.contour, -stimulus) %>% 
  cor(use = "pairwise.complete.obs") %>%
  melt() %>%
  filter(Var1 == "avg_correct") %>%
  filter(Var2 != "d.range") %>%
  filter(Var2 != "d.median") %>%
  filter(Var2 != "d.mode") %>%
  filter(Var2 != "d.entropy") %>%
  filter(Var2 != "d.eq.trans") %>%
  filter(Var2 != "d.half.trans") %>%
  filter(Var2 != "avg_correct") %>%
  filter(Var2 != "d.dotted.trans") %>%
  select(Var2, value) %>%
  mutate(Var2 = str_replace_all(string = Var2, pattern = "average_ic", replacement = "Average IDyOM IC")) %>%
  mutate(Var2 = str_replace_all(string = Var2, pattern = "Count", replacement = "MeloSol Frequency")) %>%
  mutate(feature_category = "FANTASTIC") %>% 
  rename(Feature = Var2,
         Correlation = value) %>%
  arrange(-Correlation) %>%
  mutate(absCor = abs(Correlation)) -> hand_add_categories

write_csv(hand_add_categories,path = "ffh_poster/correlations_out.csv")  
hand_in <- read_csv("ffh_poster/correlations_in.csv")  

hand_in %>%
  ggplot(aes(x = reorder(Feature,-Correlation), y = Correlation, fill = feature_category)) + 
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Univariate Feature Correlations with Average Correct",
       subtitle = "3, 5, 7, 9 grams",
       x = "Feature",
       y = "Pearson's r",
       fill = "Feature Category") +
  scale_y_continuous(limits = c(-1,1)) -> big_cor_table 

big_cor_table

# ggsave(filename = "ffh_poster/big_cor_table.png", plot = big_cor_table, device = "png")

#======================================================================================================
# Simple Regressions 
#--------------------------------------------------

modeling_table %>%
  # Only Grab Stimuli where Three or More (for Full FANTASTIC)
  filter(Gram == "Three" | Gram == "Five" | Gram == "Seven" | Gram == "Nine") %>%
  group_by(stimulus) %>%
  mutate(avg_correct = mean(score)) %>%
  select(avg_correct, Count, mean.entropy:int.cont.glob.dir, average_ic) %>%
  distinct() -> reg_data 

idyom_lm <- lm(avg_correct ~ average_ic, data = reg_data)
summary(idyom_lm)
plot(idyom_lm)

number_of_notes <- lm(avg_correct ~ len, data = reg_data)
summary(number_of_notes)

step_model <- lm(avg_correct ~ step.cont.loc.var, data = reg_data)
summary(step_model)

idyom_number_contour_lm <- lm(avg_correct ~ average_ic + len + step.cont.loc.var, data = reg_data)

#======================================================================================================
# Mixed Effects??
library(lmerTest)
library(MuMIn)
library(sjPlot)

#--------------------------------------------------
# Dependant Variable -- Score (0-100%) on each individual's partial response

#--------------------------------------------------
# Fixed Effects -- Gram (number of Notes)

#--------------------------------------------------
# Random Effects -- Participant 
#--------------------------------------------------


#--------------------------------------------------
# Get Data 
#--------------------------------------------------
modeling_table %>%
  filter(Gram == "Three" | Gram == "Five" | Gram == "Seven" | Gram == "Nine") %>%
  group_by(stimulus) %>%
  mutate(dv_score = mean(score)) %>%
  ungroup(stimulus) %>%
  select(subject,stimulus, dv_score, average_ic, Gram, Count, step.cont.loc.var, trial_length) %>%
  distinct() -> me_table

#--------------------------------------------------
# Need to Drop two More data Points

me_table %>%
  filter(stimulus != "v5 v4 v6+ ^7 ^3+ v3 v2 v5 ^4") %>% # Idyom not found 
  filter(stimulus != "v1 ^2 v1 v7 ^1 ^3 v2") %>%
  filter(stimulus != "v1 v7- v7-") %>%                   # Need global contour calculation 
  filter(stimulus != "^3 ^4 ^5 ^6 v5 v4 v3 v2 v1") -> me_table

me_table

#--------------------------------------------------
# Declare Models 
#--------------------------------------------------

me_table %>%
  select(Gram)  %>% distinct()

null_model <- lmer(dv_score ~  ( 1 | subject ), data = me_table, REML = FALSE)

anova(null_model, idyom_only_model)

# Only IDyOM 
idyom_only_model <- lmer(dv_score ~ average_ic + (1+average_ic|subject), data = me_table, REML = FALSE)

# Number of Notes 
note_only_model <- lmer(dv_score ~ trial_length + (1+trial_length|subject), data = me_table,REML = FALSE)

# Contour 
sclv_only_model <- lmer(dv_score ~ step.cont.loc.var + (1+step.cont.loc.var|subject), data = me_table,REML = FALSE)

combo <- lmer(dv_score ~ average_ic + trial_length + (1+average_ic|subject), data = me_table, REML = FALSE)

combo2 <- lmer(dv_score ~ average_ic + trial_length + step.cont.loc.var + (1+average_ic|subject), data = me_table, REML = FALSE)

#--------------------------------------------------
# Model Summaries 
plot(idyom_only_model)
summary(idyom_only_model)

plot(note_only_model)
summary(note_only_model)

plot(sclv_only_model)
summary(sclv_only_model)


#--------------------------------------------------
# Get R2 Values 

MuMIn::r.squaredGLMM(null_model)

MuMIn::r.squaredGLMM(note_only_model)
MuMIn::r.squaredGLMM(sclv_only_model)
MuMIn::r.squaredGLMM(idyom_only_model)

MuMIn::r.squaredGLMM(combo)
MuMIn::r.squaredGLMM(combo2)

#--------------------------------------------------
# ANOVAs
anova(null_model, idyom_only_model, combo, combo2)

#--------------------------------------------------
# HTML Tables

tab_model(note_only_model,
          sclv_only_model,
          idyom_only_model,
          show.r2 = TRUE,show.intercept = FALSE)


MuMIn::r.squaredGLMM(note_only_model)
MuMIn::r.squaredGLMM(sclv_only_model)
MuMIn::r.squaredGLMM(idyom_only_model)

# Paste to CSV, run thru https://www.tablesgenerator.com/latex_tables
# add resize and \hline 

#--------------------------------------------------
# Plots of All trajectories
# Facet Wrap by number of notes 

