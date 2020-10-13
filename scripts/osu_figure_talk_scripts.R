# Talk Figures to Help 

# MEAN ACC -- Frequency 
h1_table %>%
  filter(!is.na(degree_f)) %>%
  ggplot(aes(x = degree_f)) +
  # geom_point(aes(y = z_ms_total, color = "z_ms_total"),   group  = 1) +
  # geom_line(aes(y = z_ms_total,  color = "z_ms_total"),  group = 1) +
  # geom_point(aes(y = z_ms_start, color = "z_ms_start"),  group = 2) +
  # geom_line(aes(y = z_ms_start,  color = "z_ms_start"),  group = 2) +
  # geom_point(aes(y = z_essen,  color = "z_essen"),    group = 3) +
  # geom_line(aes(y = z_essen,  color = "z_essen"),     group = 3) +
  geom_point(aes(y = mean_score, color = "z_mean_score"),group = 4) +
  geom_line(aes(y = mean_score, color = "z_mean_score"), group = 4) +
  theme_minimal() +
  labs(title = "Mean Accuracy",
       x = "Scale Degree",
       y = "Mean Percent",
       color = "Legend") +
  scale_color_viridis(discrete = TRUE, labels = c(
    "Mean Accuracy of Experiment",
    "All Notes in Essen",
    "Starting Notes in MeloSol",
    "All Notes in MeloSol") ) + 
  scale_y_continuous(labels = scales::percent)-> mean_acc_pp_noz

mean_acc_pp_noz

ggsave(filename = "figures/talk/mean_acc_pp_noz.png",
       mean_acc_pp_noz,
       width = 9, height = 4, units = "in")
#------------------------------------------------------------------------------
h1_table %>%
  filter(!is.na(degree_f)) %>%
  ggplot(aes(x = degree_f)) +
  # geom_point(aes(y = z_ms_total, color = "z_ms_total"),   group  = 1) +
  # geom_line(aes(y = z_ms_total,  color = "z_ms_total"),  group = 1) +
  # geom_point(aes(y = z_ms_start, color = "z_ms_start"),  group = 2) +
  # geom_line(aes(y = z_ms_start,  color = "z_ms_start"),  group = 2) +
  # geom_point(aes(y = z_essen,  color = "z_essen"),    group = 3) +
  # geom_line(aes(y = z_essen,  color = "z_essen"),     group = 3) +
  geom_point(aes(y = z_mean_score, color = "z_mean_score"),group = 4) +
  geom_line(aes(y = z_mean_score, color = "z_mean_score"), group = 4) +
  theme_minimal() +
  labs(title = "Normalized Scale Degree Frequency and Mean Accuracy",
       x = "Scale Degree",
       y = "z score",
       color = "Legend") +
  scale_color_viridis(discrete = TRUE, labels = c(
    "Mean Accuracy of Experiment",
    "All Notes in Essen",
    "Starting Notes in MeloSol",
    "All Notes in MeloSol") ) -> mean_acc_pp

mean_acc_pp

ggsave(filename = "figures/talk/mean_acc_pp.png",
       mean_acc_pp,
       width = 9, height = 4, units = "in")

#---------------------------------------------------------------------------
# ESSEN -- RAW

h1_table %>%
  filter(!is.na(degree_f)) %>%
  ggplot(aes(x = degree_f)) +
  # geom_point(aes(y = z_ms_total, color = "z_ms_total"),   group  = 1) +
  # geom_line(aes(y = z_ms_total,  color = "z_ms_total"),  group = 1) +
  # geom_point(aes(y = z_ms_start, color = "z_ms_start"),  group = 2) +
  # geom_line(aes(y = z_ms_start,  color = "z_ms_start"),  group = 2) +
  geom_point(aes(y = count,  color = "All Notes in Essen"),    group = 3) +
  geom_line(aes(y = count,  color = "All Notes in Essen"),     group = 3) +
  # geom_point(aes(y = mean_score, color = "z_mean_score"),group = 4) +
  # geom_line(aes(y = mean_score, color = "z_mean_score"), group = 4) +
  theme_minimal() +
  labs(title = "Essen Distribution",
       x = "Scale Degree",
       y = "Frequency Count",
       color = "Legend") +
  scale_color_viridis(discrete = TRUE, labels = c("All Notes in Essen"), begin = .25, end = .25 ) -> essen_plot_help

essen_plot_help

ggsave(filename = "figures/talk/essen_plot_help.png",
       essen_plot_help,
       width = 9, height = 4, units = "in")
#-----------------------------------------------------------------------
h1_table %>%
  filter(!is.na(degree_f)) %>%
  ggplot(aes(x = degree_f)) +
  # geom_point(aes(y = z_ms_total, color = "z_ms_total"),   group  = 1) +
  # geom_line(aes(y = z_ms_total,  color = "z_ms_total"),  group = 1) +
  # geom_point(aes(y = z_ms_start, color = "z_ms_start"),  group = 2) +
  # geom_line(aes(y = z_ms_start,  color = "z_ms_start"),  group = 2) +
  geom_point(aes(y = z_essen,  color = "All Notes in Essen"),    group = 3) +
  geom_line(aes(y = z_essen,  color = "All Notes in Essen"),     group = 3) +
  # geom_point(aes(y = mean_score, color = "z_mean_score"),group = 4) +
  # geom_line(aes(y = mean_score, color = "z_mean_score"), group = 4) +
  theme_minimal() +
  labs(title = "Essen Distribution",
       x = "Scale Degree",
       y = "z score",
       color = "Legend") +
  scale_color_viridis(discrete = TRUE, labels = c("All Notes in Essen"), begin = .25, end = .25 ) -> essen_plot_help_z

essen_plot_help_z

ggsave(filename = "figures/talk/essen_plot_help_z.png",
       essen_plot_help_z,
       width = 9, height = 4, units = "in")

#-----------------------------------------------------------------------

h1_table %>%
  filter(!is.na(degree_f)) %>%
  ggplot(aes(x = degree_f)) +
  # geom_point(aes(y = z_ms_total, color = "z_ms_total"),   group  = 1) +
  # geom_line(aes(y = z_ms_total,  color = "z_ms_total"),  group = 1) +
  geom_point(aes(y = melody_start_counts, color = "z_ms_start"),  group = 2) +
  geom_line(aes(y = melody_start_counts,  color = "z_ms_start"),  group = 2) +
  # geom_point(aes(y = count,  color = "All Notes in Essen"),    group = 3) +
  # geom_line(aes(y = count,  color = "All Notes in Essen"),     group = 3) +
  # geom_point(aes(y = mean_score, color = "z_mean_score"),group = 4) +
  # geom_line(aes(y = mean_score, color = "z_mean_score"), group = 4) +
  theme_minimal() +
  labs(title = "MeloSol Starting Notes Distribution",
       x = "Scale Degree",
       y = "Frequency Count",
       color = "Legend") +
  scale_color_viridis(discrete = TRUE, labels = c("Starting Notes of MeloSol"), 
                      begin = .5, end = .5 ) -> ms_starter_plot

ms_starter_plot

ggsave(filename = "figures/talk/ms_starter_plot.png",
       ms_starter_plot,
       width = 9, height = 4, units = "in")

#-----------------------------------------------------------------------
h1_table %>%
  filter(!is.na(degree_f)) %>%
  ggplot(aes(x = degree_f)) +
  geom_point(aes(y = melosol_total_count, color = "z_ms_total"),   group  = 1) +
  geom_line(aes(y = melosol_total_count,  color = "z_ms_total"),  group = 1) +
  # geom_point(aes(y = z_ms_start, color = "z_ms_start"),  group = 2) +
  # geom_line(aes(y = z_ms_start,  color = "z_ms_start"),  group = 2) +
  # geom_point(aes(y = z_essen,  color = "z_essen"),    group = 3) +
  # geom_line(aes(y = z_essen,  color = "z_essen"),     group = 3) +
  # geom_point(aes(y = z_mean_score, color = "z_mean_score"),group = 4) +
  # geom_line(aes(y = z_mean_score, color = "z_mean_score"), group = 4) +
  theme_minimal() +
  labs(title = "MeloSol Total Count",
       x = "Scale Degree",
       y = "Frequency",
       color = "Legend") +
  scale_color_viridis(discrete = TRUE, begin = .75, end = .75, labels = c("All Notes in MeloSol") ) -> ms_all

ms_all

ggsave(filename = "figures/talk/ms_all.png",
       ms_all,
       width = 9, height = 4, units = "in")

