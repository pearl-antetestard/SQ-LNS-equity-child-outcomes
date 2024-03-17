### Growth outcomes and SEP
### This script is part of the project "Equity in the effect of SQ-LNS
### on child outcomes in the Global South: an analysis of individual participant data" # nolint
### Author: Pearl Ante-Testard
### Date: 2024-03-15
### E-mail: pearl.ante@ucsf.edu


# Clean workspace
rm(list=ls(all=TRUE)) # nolint

# Load libraries
library(mgcv)
library(ggeffects)
library(DHARMa)
library(mgcViz)
library(geepack)
library(msm)
library(tidymv)
#remotes::install_github("stefanocoretta/tidygam@devel") # nolint
library(tidygam)
library(cowplot)
library(ggpubr)
library(lmtest)
library(gridExtra)
library(here)
library(haven)
library(ggplot2)
library(dplyr)
library(viridis)

# Load data and create new variables
df_analysis <- read_dta(file = here::here("data", "IPD_full_20200905.dta")) %>%
  df_analysis <- df_analysis %>% # nolint
    mutate(severe_stunted_bl = ifelse(laz_bl < -3, 1, 0)) %>% # severe stunting at baseline # nolint
    mutate(severe_stunted_bl = factor(severe_stunted_bl, levels=c(0,1), labels=c("0","1"))) %>% # nolint
    mutate(severe_wasted_bl = ifelse(wlz_bl < -3, 1, 0)) %>% # severe wasting at baseline # nolint
    mutate(severe_wasted_bl = factor(severe_wasted_bl, levels=c(0,1), labels=c("0","1"))) %>% # nolint
    mutate(study = sub("s[0-9]*_", "", study))

# Create a new variable for wealth rank
df_analysis <- df_analysis %>%
  group_by(study) %>% # nolint
  mutate(wealth_rank = rank(sesindex) / n()) %>%
  ungroup()

# Create a boxplot of wealth rank by study
p <- ggplot(df_analysis, aes(x = study, y = wealth_rank)) +
  geom_boxplot() +
  labs(x = "Study", y = "Wealth rank", title = "Wealth rank by study") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
      axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability # nolint
      legend.position = "none") # Remove legend

# Save the plot as a PNG file
ggsave(here::here("output", "wealth_rank_by_study.png"), plot = p, width = 10, height = 6, dpi = 300) # nolint

# Create a scatterplot of LAZ at baseline by study
laz_bl_plot <- ggplot(df_analysis, aes(x = study, y = laz_bl)) +
  scale_y_continuous(breaks=c(-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6), labels=c("-6","-5","-4","-3","-2","1", # nolint
  "0","1","2","3","4","5","6")) + # nolint
  geom_boxplot() +
  labs(x = "Study", y = "LAZ at baseline", title = "LAZ at baseline by study") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
    axis.text.x = element_text(angle = 45, hjust = 1), # nolint
    legend.position = "none") # Rotate x-axis labels for better readability

laz_el_plot <- ggplot(df_analysis, aes(x = study, y = laz_el)) +
  scale_y_continuous(breaks=c(-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6), labels=c("-6","-5","-4","-3","-2","-1", # nolint
  "0","1","2","3","4","5","6")) + # nolint
  geom_boxplot() +
  labs(x = "Study", y = "LAZ at endline", title = "LAZ at endline by study") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
    axis.text.x = element_text(angle = 45, hjust = 1), # nolint
    legend.position = "none") # Rotate x-axis labels for better readability

# Merge the plots
laz_bl_el_plot <- plot_grid(laz_bl_plot, laz_el_plot, ncol = 2, align = "vh")

# Save the plot as a PNG file
ggsave(here::here("output", "laz_bl_el_by_study.png"), plot = laz_bl_el_plot, width = 10, height = 6, dpi = 300) # nolint

# Create a scatterplot of LAZ at baseline by wealth rank for each study
q_bl <- ggplot(df_analysis, aes(x = wealth_rank, y = laz_bl)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  facet_wrap(~study) +
  labs(x = "Wealth Rank", y = "LAZ at Baseline", title = "LAZ at Baseline by Wealth Rank for each Study") + # nolint
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
      axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability # nolint

# Save the plot as a PNG file
ggsave(here::here("output", "laz_bl_by_wealth_rank.png"), plot = q_bl, width = 10, height = 6, dpi = 300) # nolint

# Create a scatterplot of LAZ at endline by wealth rank for each study
q_el <- ggplot(df_analysis, aes(x = wealth_rank, y = laz_el)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  facet_wrap(~study) +
  labs(x = "Wealth rank", y = "LAZ at endline", title = "LAZ at endline by wealth rank for each study") + # nolint # nolint
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
   axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability # nolint

# Save the plot as a PNG file
ggsave(here::here("output", "laz_el_by_wealth_rank.png"), plot = q_el, width = 10, height = 6, dpi = 300) # nolint
