#======================================================================================================
# Update Graphics
#--------------------------------------------------
library(tidyverse)
library(cowplot)
library(viridis)
#--------------------------------------------------
# REMEMBER TO SET VIEWER SCREEN AHEAD OF TIME!!!
#--------------------------------------------------
# Source Scripts that make ggplots
#--------------------------------------------------
source("analyses/01_update_raw_data.R")
source("analyses/02_demo_analysis.R")
source("analyses/03_single_tone_eda.R")
source("analyses/04_single_tone_corpus.R")
source("analyses/05_multi_tone_EDA.R")
#--------------------------------------------------
# Demographic Plots (02)
ggsave(filename = "ffh_poster/demo_plot1.png",x = demo_plot_1, device = "png")

#--------------------------------------------------
# Single Tones
ggsave(filename = "ffh_poster/sdrt_big.png",plot = sdrt_big,device = "png")

ggsave(filename = "ffh_poster/sdrt_big_indv.png",
       plot = sdrt_big_indv,
       device = "png")

ggsave(filename = "ffh_poster/sdrt_big_key.png",plot = sdrt_big_key,device = "png")

ggsave(filename = "ffh_poster/sdrt_big_key_correct.png",
       plot = sdrt_big_key_correct,
       device = "png")

ggsave(filename = "ffh_poster/sdrt_big_key_note.png",
       plot = sdrt_big_key_note,
       device = "png")


ggsave(filename = "ffh_poster/sdrt_big_key_note.png",
       plot = sdrt_big_key_note,
       device = "png")

ggsave(filename = "ffh_poster/sdrt_big_key_note_correct.png",
       plot = sdrt_big_key_note_correct,
       device = "png")

ggsave(filename = "ffh_poster/krumhansl_1982.png",
       plot = krumhansl_1982,
       device = "png")

ggsave(filename = "ffh_poster/krumhansl_multi_plot.png",
       plot = krum_mutli_plot,
 device = "png")

#--------------------------------------------------
# Single Tone Corpus Plots

ggsave(filename = "ffh_poster/ffh_rt.png",plot = ffh_rt,device = "png")

ggsave(filename = "ffh_poster/ffh_correct.png",plot = ffh_correct,device = "png")

ggsave(filename = "ffh_poster/ffh_correct_lm.png",plot = ffh_correct_lm,device = "png")

#--------------------------------------------------
# Multi Tone

ggsave(filename = "ffh_poster/grouped_idyom_regression.png",plot = grouped_idyom_regression_plot,device = "png")

ggsave(filename = "ffh_poster/dual_plot.png", dual_plot, device = "png")

ggsave(filename = "ffh_poster/item_map.png",plot = item_difficulty_map, device = "png")

