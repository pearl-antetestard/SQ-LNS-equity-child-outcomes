### Anthropometric measures and SEP
### This script is part of the project "Equity in the effect of SQ-LNS on child outcomes in the Global South: an analysis of individual participant data"
### Author: Pearl Ante-Testard
### Date: 2024-03-15
### E-mail: pearl.ante@ucsf.edu


# Clean workspace
rm(list=ls(all=TRUE))

# Load libraries
library(mgcv)
library(ggeffects)
library(DHARMa)
library(mgcViz)
library(geepack)
library(msm)
library(tidymv)
#remotes::install_github("stefanocoretta/tidygam@devel")
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
                                 mutate(severe_stunted_bl = ifelse(laz_bl < -3, 1, 0)) %>% # severe stunting at baseline
                                 mutate(severe_stunted_bl = factor(severe_stunted_bl, levels=c(0,1), labels=c("0","1"))) %>%
                                 mutate(severe_wasted_bl = ifelse(wlz_bl < -3, 1, 0)) %>% # severe wasting at baseline
                                 mutate(severe_wasted_bl = factor(severe_wasted_bl, levels=c(0,1), labels=c("0","1"))) %>%
                                 mutate(study = sub("s[0-9]*_", "", study))

# Create a new variable for wealth rank
df_analysis <- df_analysis %>%
      group_by(study) %>%
      mutate(wealth_rank = rank(sesindex) / n()) %>%
      ungroup()

# Create a boxplot of wealth rank by study
p <- ggplot(df_analysis, aes(x = study, y = wealth_rank)) +
  geom_boxplot() +
  labs(x = "Study", y = "Wealth rank", title = "Wealth rank by study") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
        axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability
        legend.position = "none") # Remove legend

# Save the plot as a PNG file
ggsave(here::here("output", "wealth_rank_by_study.png"), plot = p, width = 10, height = 6, dpi = 300)


# Create a scatterplot of LAZ at baseline by wealth rank for each study
q_bl <- ggplot(df_analysis, aes(x = wealth_rank, y = laz_bl)) +
      geom_point(alpha = 0.5) +
      facet_wrap(~study) +
      labs(x = "Wealth Rank", y = "LAZ at Baseline", title = "LAZ at Baseline by Wealth Rank for each Study") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5), # Center the plot title
                        axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

# Save the plot as a PNG file
ggsave(here::here("output", "laz_bl_by_wealth_rank.png"), plot = q_bl, width = 10, height = 6, dpi = 300)
