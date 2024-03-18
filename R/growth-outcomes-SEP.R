### Growth outcomes and SEP
### This script is part of the project "Equity in the effect of SQ-LNS
### on child outcomes in the Global South: an analysis of individual participant data" # nolint
### Author: Pearl Ante-Testard
### Date: 2024-03-15
### E-mail: pearl.ante@ucsf.edu


# Clean workspace
rm(list=ls(all=TRUE)) # nolint

# Load libraries
library(cowplot)
library(here)
library(haven)
library(ggplot2)
library(dplyr)

################## Load data ###################

df_analysis <- readRDS(here::here("data", "1-final",
                                  "IPD_full_20200905_growth_formatted.RDS")) # nolint

################### Wealth rank by study ###################

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

################### LAZ by study ###################

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

# Create a scatterplot of LAZ at endline by study
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


################### WLZ by study ###################

# Create a scatterplot of WLZ at baseline by study
wlz_bl_plot <- ggplot(df_analysis, aes(x = study, y = wlz_bl)) +
  scale_y_continuous(breaks=c(-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6), labels=c("-6","-5","-4","-3","-2","-1", # nolint
  "0","1","2","3","4","5","6")) + # nolint
  geom_boxplot() +
  labs(x = "Study", y = "WLZ at baseline", title = "WLZ at baseline by study") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
    axis.text.x = element_text(angle = 45, hjust = 1), # nolint
    legend.position = "none") # Rotate x-axis labels for better readability

# Create a scatterplot of WLZ at endline by study
wlz_el_plot <- ggplot(df_analysis, aes(x = study, y = wlz_el)) +
  scale_y_continuous(breaks=c(-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6), labels=c("-6","-5","-4","-3","-2","-1", # nolint
  "0","1","2","3","4","5","6")) + # nolint
  geom_boxplot() +
  labs(x = "Study", y = "WLZ at endline", title = "WLZ at endline by study") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
    axis.text.x = element_text(angle = 45, hjust = 1), # nolint
    legend.position = "none") # Rotate x-axis labels for better readability

# Merge the plots
wlz_bl_el_plot <- plot_grid(wlz_bl_plot, wlz_el_plot, ncol = 2, align = "vh")

# Save the plot as a PNG file
ggsave(here::here("output", "wlz_bl_el_by_study.png"), plot = wlz_bl_el_plot, width = 10, height = 6, dpi = 300) # nolint

# Create a scatterplot of WLZ at baseline by wealth rank for each study
r_bl <- ggplot(df_analysis, aes(x = wealth_rank, y = wlz_bl)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  facet_wrap(~study) +
  labs(x = "Wealth rank", y = "WLZ at baseline", title = "WLZ at baseline by wealth rank for each study") + # nolint
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
      axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability # nolint

# Save the plot as a PNG file
ggsave(here::here("output", "wlz_bl_by_wealth_rank.png"), plot = r_bl, width = 10, height = 6, dpi = 300) # nolint

# Create a scatterplot of WLZ at endline by wealth rank for each study
r_el <- ggplot(df_analysis, aes(x = wealth_rank, y = wlz_el)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  facet_wrap(~study) +
  labs(x = "Wealth rank", y = "WLZ at endline", title = "WLZ at endline by wealth rank for each study") + # nolint
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
      axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability # nolint

# Save the plot as a PNG file
ggsave(here::here("output", "wlz_el_by_wealth_rank.png"), plot = r_el, width = 10, height = 6, dpi = 300) # nolint


################### Severe stunted by study ###################
# Filter the data to only include rows where severe_stunted_bl is "1"
df_analysis_filtered_ss <- df_analysis[df_analysis$severe_stunted_bl == 1,]
df_analysis_filtered_ss <- df_analysis_filtered_ss %>%
                           filter(!is.na(severe_stunted_bl))

# Create a barplot of severe stunting at baseline by study
severe_stunted_bl_plot <- ggplot(df_analysis_filtered_ss, aes(x = study, fill = severe_stunted_bl)) + # nolint
  geom_bar(position = "dodge") +
  labs(x = "Study", y = "Count", title = "Severe stunting at baseline by study") + # nolint
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
      axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability # nolint
      legend.position = "none") # Remove legend


# Filter the data to only include rows where severe_stunted_el is "1"
df_analysis_filtered_ss <- df_analysis[df_analysis$severe_stunted_el == 1,]
df_analysis_filtered_ss <- df_analysis_filtered_ss %>%
                           filter(!is.na(severe_stunted_el))

# Create a barplot of severe stunting at endline by study
severe_stunted_el_plot <- ggplot(df_analysis_filtered_ss, aes(x = study, fill = severe_stunted_el)) + # nolint
  geom_bar(position = "dodge") +
  labs(x = "Study", y = "Count", title = "Severe stunting at endline by study") + # nolint
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
      axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability # nolint
      legend.position = "none") # Remove legend

# Merge the plots
severe_stunted_bl_el_plot <- plot_grid(severe_stunted_bl_plot, severe_stunted_el_plot, ncol = 2, align = "vh") # nolint

# Save the plot as a PNG file
ggsave(here::here("output", "severe_stunted_bl_el_by_study.png"), plot = severe_stunted_bl_el_plot, width = 10, height = 6, dpi = 300) # nolint


################### Severe wasting by study ###################
# Filter the data to only include rows where severe_wasted_bl is "1"
df_analysis_filtered_sw <- df_analysis[df_analysis$severe_wasted_bl == 1,]
df_analysis_filtered_sw <- df_analysis_filtered_sw %>%
                           filter(!is.na(severe_wasted_bl))

# Create a barplot of severe wasting at baseline by study
severe_wasted_bl_plot <- ggplot(df_analysis_filtered_sw, aes(x = study, fill = severe_wasted_bl)) + # nolint
  geom_bar(position = "dodge") +
  labs(x = "Study", y = "Count", title = "Severe wasting at baseline by study") + # nolint
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
      axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability # nolint
      legend.position = "none") # Remove legend

# Filter the data to only include rows where severe_wasted_el is "1"
df_analysis_filtered_sw <- df_analysis[df_analysis$severe_wasted_el == 1,]
df_analysis_filtered_sw <- df_analysis_filtered_sw %>%
                           filter(!is.na(severe_wasted_el))

# Create a barplot of severe wasting at endline by study
severe_wasted_el_plot <- ggplot(df_analysis_filtered_sw, aes(x = study, fill = severe_wasted_el)) + # nolint
  geom_bar(position = "dodge") +
  labs(x = "Study", y = "Count", title = "Severe wasting at endline by study") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
      axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability # nolint
      legend.position = "none") # Remove legend

# Merge the plots
severe_wasted_bl_el_plot <- plot_grid(severe_wasted_bl_plot, severe_wasted_el_plot, ncol = 2, align = "vh") # nolint

# Save the plot as a PNG file
ggsave(here::here("output", "severe_wasted_bl_el_by_study.png"), plot = severe_wasted_bl_el_plot, width = 10, height = 6, dpi = 300) # nolint




################### Prevalences ###################

#### Mean LAZ at baseline and endline by study ####

# Create a new prevalence variables by study
df_analysis_prev <- df_analysis %>%
  group_by(study) %>%
  mutate(laz_bl_mean = mean(laz_bl, na.rm = TRUE),
         laz_el_mean = mean(laz_el, na.rm = TRUE),
         wlz_bl_mean = mean(wlz_bl, na.rm = TRUE),
         wlz_el_mean = mean(wlz_el, na.rm = TRUE),
         severe_stunted_bl_prevalence = sum(severe_stunted_bl == 1, na.rm = TRUE) / n() * 100,
         severe_stunted_el_prevalence = sum(severe_stunted_el == 1, na.rm = TRUE) / n() * 100,
         severe_wasted_bl_prevalence = sum(severe_wasted_bl == 1, na.rm = TRUE) / n() * 100,
         severe_wasted_el_prevalence = sum(severe_wasted_el == 1, na.rm = TRUE) / n() * 100) %>%
  ungroup()

# Mean LAZ at baseline
laz_bl_prev <- ggplot(df_analysis_prev, aes(x = study, y = laz_bl_mean)) +
  geom_bar(stat = "summary") +
  labs(x = "Study", y = "Mean", title = "Mean LAZ at baseline by study") + # nolint
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
      axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability # nolint
      legend.position = "none") # Remove legend

# Mean LAZ at endline
laz_el_prev <- ggplot(df_analysis_prev, aes(x = study, y = laz_el_mean)) +
  geom_bar(stat = "summary") +
  labs(x = "Study", y = "Mean", title = "Mean LAZ at endline by study") + # nolint
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
      axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability # nolint
      legend.position = "none") # Remove legend

# Merge the plots
laz_bl_el_prev <- plot_grid(laz_bl_prev, laz_el_prev, ncol = 2, align = "vh")

# Save the plot as a PNG file
ggsave(here::here("output", "laz_bl_el_mean_by_study.png"), plot = laz_bl_el_prev, width = 10, height = 6, dpi = 300) # nolint

#### Mean WLZ at baseline and endline by study ####

# Mean WLZ at baseline 
wlz_bl_prev <- ggplot(df_analysis_prev, aes(x = study, y = wlz_bl_mean)) +
  geom_bar(stat = "summary") +
  labs(x = "Study", y = "Mean", title = "Mean WLZ at baseline by study") + # nolint
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
      axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability # nolint
      legend.position = "none") # Remove legend

# Mean WLZ at endline
wlz_el_prev <- ggplot(df_analysis_prev, aes(x = study, y = wlz_el_mean)) +
  geom_bar(stat = "summary") +
  labs(x = "Study", y = "Mean", title = "Mean WLZ at endline by study") + # nolint
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
      axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability # nolint
      legend.position = "none") # Remove legend

# Merge the plots
wlz_bl_el_prev <- plot_grid(wlz_bl_prev, wlz_el_prev, ncol = 2, align = "vh")

# Save the plot as a PNG file
ggsave(here::here("output", "wlz_bl_el_mean_by_study.png"), plot = wlz_bl_el_prev, width = 10, height = 6, dpi = 300) # nolint

#### Prevalence of severe stunting at baseline and endline by study ####

# Prevalence of severe stunting at baseline
severe_stunted_bl_prev <- ggplot(df_analysis_prev, aes(x = study, y = severe_stunted_bl_prevalence)) +
  geom_bar(stat = "summary") +
  labs(x = "Study", y = "Prevalence (%)", title = "Prevalence of severe stunting at baseline by study") + # nolint
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
      axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability # nolint
      legend.position = "none") # Remove legend

# Prevalence of severe stunting at endline
severe_stunted_el_prev <- ggplot(df_analysis_prev, aes(x = study, y = severe_stunted_el_prevalence)) +
  geom_bar(stat = "summary") +
  labs(x = "Study", y = "Prevalence (%)", title = "Prevalence of severe stunting at endline by study") + # nolint
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
      axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability # nolint
      legend.position = "none") # Remove legend

# Merge the plots 
severe_stunted_bl_el_prev <- plot_grid(severe_stunted_bl_prev, severe_stunted_el_prev, ncol = 2, align = "vh")

# Save the plot as a PNG file
ggsave(here::here("output", "severe_stunted_bl_el_prev_by_study.png"), plot = severe_stunted_bl_el_prev, width = 10, height = 6, dpi = 300) # nolint

#### Prevalence of severe wasting at baseline and endline by study ####

# Prevalence of severe wasting at baseline
severe_wasted_bl_prev <- ggplot(df_analysis_prev, aes(x = study, y = severe_wasted_bl_prevalence)) +
  geom_bar(stat = "summary") +
  labs(x = "Study", y = "Prevalence (%)", title = "Prevalence of severe wasting at baseline by study") + # nolint
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
      axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability # nolint
      legend.position = "none") # Remove legend

# Prevalence of severe wasting at endline
severe_wasted_el_prev <- ggplot(df_analysis_prev, aes(x = study, y = severe_wasted_el_prevalence)) +
  geom_bar(stat = "summary") +
  labs(x = "Study", y = "Prevalence (%)", title = "Prevalence of severe wasting at endline by study") + # nolint
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
      axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability # nolint
      legend.position = "none") # Remove legend

# Merge the plots
severe_wasted_bl_el_prev <- plot_grid(severe_wasted_bl_prev, severe_wasted_el_prev, ncol = 2, align = "vh")

# Save the plot as a PNG file
ggsave(here::here("output", "severe_wasted_bl_el_prev_by_study.png"), plot = severe_wasted_bl_el_prev, width = 10, height = 6, dpi = 300) # nolint


### End of script