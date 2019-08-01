#--------------------------------------------------
# Multi 

#======================================================================================================
# plot all the per stimuli, then overlay the IC !!!!


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

#=====================================================================================================
