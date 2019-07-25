#--------------------------------------------------
# Demographic Analysis Script
#--------------------------------------------------
# This script creates Demographic table 
# as well as the plot used for gender and age 
#--------------------------------------------------
library(tidyverse)
library(viridis)
#--------------------------------------------------
# Import 
demo_data <- read_csv("data/aggregate_data/current_demo_table.csv")
#--------------------------------------------------
# Create Current Demo Table 

demo_data %>%
  mutate(new_age = str_remove_all(string = age, pattern = "\\{\\\\Q0\\\\\\:\\\\")) %>%
  mutate(new_age = str_remove_all(string = new_age, pattern = "\\\\\\}")) %>%
  mutate(age = as.numeric(new_age)) %>%
  mutate(education = str_remove_all(string = education, pattern = "\\{\\\\Q0\\\\\\:\\\\")) %>%
  mutate(education = str_remove_all(string = education, pattern = "\\\\\\}")) %>%
  mutate(gender = str_remove_all(string = gender, pattern = "\\{\\\\Q0\\\\\\:\\\\")) %>%
  mutate(gender = str_remove_all(string = gender, pattern = "\\\\\\}")) %>%
  mutate(AP = str_remove_all(string = AP, pattern = "\\{\\\\Q0\\\\\\:\\\\")) %>%
  mutate(AP = str_remove_all(string = AP, pattern = "\\\\\\}")) %>%
  mutate(weeks_taking = str_remove_all(string = weeks_taking, pattern = "\\{\\\\Q0\\\\\\:\\\\")) %>%
  mutate(weeks_taking = str_remove_all(string = weeks_taking, pattern = "\\\\\\}")) %>%
  mutate(years_teaching = str_remove_all(string = years_teaching, pattern = "\\{\\\\Q0\\\\\\:\\\\")) %>%
  mutate(years_teaching = str_remove_all(string = years_teaching, pattern = "\\\\\\}")) %>%
  mutate(gender = str_to_upper(gender)) %>%
  mutate(gender = str_replace_all(string = gender, pattern = "\\F$", replacement = "FEMALE")) %>%
  select(subject_id,age, education, gender, AP, weeks_taking, years_teaching) -> d_table

d_table

number_participants <- nrow(d_table)
#--------------------------------------------------
# Create Demo Chart 

d_table %>%
  summarise(mean_age = mean(age), sd_age = sd(age)) -> age_stats

mean_age <- round(age_stats[1],2)
sd_age <- round(age_stats[2],2)

demo_data %>%
  mutate(new_age = str_remove_all(string = age, pattern = "\\{\\\\Q0\\\\\\:\\\\")) %>%
  mutate(new_age = str_remove_all(string = new_age, pattern = "\\\\\\}")) %>%
  mutate(age = as.numeric(new_age)) %>%
  mutate(education = str_remove_all(string = education, pattern = "\\{\\\\Q0\\\\\\:\\\\")) %>%
  mutate(education = str_remove_all(string = education, pattern = "\\\\\\}")) %>%
  mutate(gender = str_remove_all(string = gender, pattern = "\\{\\\\Q0\\\\\\:\\\\")) %>%
  mutate(gender = str_remove_all(string = gender, pattern = "\\\\\\}")) %>%
  mutate(AP = str_remove_all(string = AP, pattern = "\\{\\\\Q0\\\\\\:\\\\")) %>%
  mutate(AP = str_remove_all(string = AP, pattern = "\\\\\\}")) %>%
  mutate(weeks_taking = str_remove_all(string = weeks_taking, pattern = "\\{\\\\Q0\\\\\\:\\\\")) %>%
  mutate(weeks_taking = str_remove_all(string = weeks_taking, pattern = "\\\\\\}")) %>%
  mutate(years_teaching = str_remove_all(string = years_teaching, pattern = "\\{\\\\Q0\\\\\\:\\\\")) %>%
  mutate(years_teaching = str_remove_all(string = years_teaching, pattern = "\\\\\\}")) %>%
  mutate(gender = str_to_upper(gender)) %>%
  mutate(gender = str_replace_all(string = gender, pattern = "\\F$", replacement = "FEMALE")) %>%
  select(age, education, gender, AP, weeks_taking, years_teaching) %>%
  ggplot(aes(x = age, fill = gender)) +
  geom_density(alpha = .5) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Age and Gender Distribution in Sample",
       x = "Age",
       y =  "Density",
       fill = "Gender",
       subtitle = paste("N =",number_participants,"Mean =", mean_age, "SD = ", sd_age)) +
  theme_minimal() -> demo_plot_1

demo_plot_1

# ggsave(filename = "ffh_poster/demo_plot1.png",x =demo_plot_1, device = "png")