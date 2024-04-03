### Effect of SQ-LNS on LAZ by wealth between the intervention and control groups - simplified version # nolint
### Author: Pearl Ante-Testard
### Date: 2024-03-27

# Clean workspace
rm(list = ls(all = TRUE))

# Load libraries
library(tidyverse)
#remotes::install_github("stefanocoretta/tidygam@devel", force = TRUE) # nolint
library(mgcv)
library(here)
library(haven)
library(ggpubr)
library(ggrepel)

# Load data
df_analysis <- readRDS(here::here("data", "1-final",
                                  "IPD_full_20240221_growth_formatted.RDS")) %>%
               select(study, pid, cluster, arm_primary, arms_maternal, laz_el, wealth_rank) %>% # nolint
               dplyr::filter(!is.na(laz_el)) %>%
               dplyr::filter(!is.na(wealth_rank)) %>%
               #dplyr::filter(!is.na(arm_primary)) %>%
               dplyr::filter(!is.na(arms_maternal)) %>%
               mutate(arm_primary = factor(arm_primary),
                      arms_maternal = factor(arms_maternal),
                      study = factor(study),
                      laz_el = as.numeric(laz_el),
                      cluster = factor(cluster),
                      dummy = rep(1, n()))

#*** Note: DYADG and DYADM have empty rows for arm_primary -- why?

# Get the smooth differences
smooth_differences <- df_analysis %>%
  split(.$study) %>%
  map(function(df_analysis) {
    study_name <- unique(df_analysis$study)
    if (study_name %in% c("DOSE", "GHANA", "HAITI", "DYADG", "DYADM")) {
      model = gam(laz_el ~ arms_maternal + s(wealth_rank, bs = "cr", by = arms_maternal), # nolint
                  data = df_analysis,
                  family = "gaussian", method = "REML")
    } else {
      model = gam(laz_el ~ arms_maternal + s(wealth_rank, bs = "cr", by = arms_maternal) + # nolint
                    s(cluster, bs = "re", by = dummy),
                  data = df_analysis,
                  family = "gaussian", method = "REML")
    }
    smooth_difference <- get_smooths_difference(model, wealth_rank,
                                                difference = list(arms_maternal = c("1","0"))) # nolint
    data.frame(study = study_name, smooth_difference)
  }) %>%
  bind_rows()

# Fit the overall GAM model to the pooled data
overall_model <- gam(laz_el ~ arms_maternal + s(wealth_rank, bs = "cr", by = arms_maternal) + # nolint
                       s(cluster, bs = "re", by = dummy),
                     data = df_analysis,
                     family = "gaussian", method = "REML")

# Get the smooth differences for the overall model
overall_smooth_difference <- get_smooths_difference(overall_model, wealth_rank,
                                                    difference = list(arms_maternal = c("1","0"))) # nolint

# Add the overall smooth differences to the smooth_differences data frame
smooth_differences <- smooth_differences %>%
  rbind(data.frame(study = "Overall", overall_smooth_difference))

# Plot the smooth differences

# Get the last point of each study
last_points <- smooth_differences %>%
  group_by(study) %>%
  filter(wealth_rank == max(wealth_rank))

# Filter the data for the "Overall" study
smooth_differences_overall <- smooth_differences %>%
  filter(study == "Overall")

# Generate a color palette
color_palette <- hcl.colors(18, "Set3")

# Get the unique studies
studies <- unique(smooth_differences$study)

# Assign colors to studies
names(color_palette) <- studies

# Change the color for "Overall" to grey
color_palette["Overall"] <- "black"

plot_diff <- ggplot(smooth_differences, aes(x = wealth_rank, y = difference, color = study)) + # nolint
  geom_line(aes(), linewidth = 0.7) +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  geom_ribbon(data = smooth_differences_overall, aes(ymin = CI_lower, ymax = CI_upper, color = "grey"), alpha = 0.2) + # nolint
  geom_text_repel(data = last_points, aes(label = study), hjust = -0.1) +
  labs(x = "Wealth rank based on the wealth index score",
       y = "Endline LAZ mean difference (SQ-LNS - control)") +
  scale_y_continuous(breaks = seq(min(smooth_differences$difference), max(smooth_differences$difference), by = 0.1), # nolint
                     labels = function(x) round(x, 2)) +
  scale_color_manual(values = color_palette) +
  theme_minimal() +
  theme(legend.position = "none")

# Save the plot
ggsave(here::here("output", "gam_effect_sqlns_laz_sep_simplified.png"),
       plot = plot_diff, width = 8, height = 6, dpi = 300)
