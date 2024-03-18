### Effect of SQ-LNS on LAZ by wealth between the intervention and control groups
### Author: Pearl Ante-Testard
### Date: 2024-03-17


# Clean workspace
rm(list=ls(all=TRUE))

# Load libraries
library(tidyverse)
#remotes::install_github("stefanocoretta/tidygam@devel")
library(tidymv)
library(mgcv)
library(here)
library(haven)
library(ggpubr)

# Load data
df_analysis <- readRDS(here::here("data", "1-final",
                      "IPD_full_20200905_growth_formatted.RDS")) %>%
         select(study, pid, cluster, arm_primary, laz_el, wealth_rank) %>%
         dplyr::filter(!is.na(laz_el)) %>%
         dplyr::filter(!is.na(wealth_rank)) %>%
         dplyr::filter(!is.na(arm_primary)) %>%
         mutate(arm_primary = factor(arm_primary),
             study = factor(study),
             laz_el = as.numeric(laz_el)) 

# Find out which studies have values for the cluster variable
df_analysis %>%
  group_by(study) %>%
  summarise(n = n(), .groups = "drop")

# Conduct the gam() analysis for each study with a random effect for only those studies 
# with a value in the cluster variable

######### Studies that were randomized at the individual level #########
########################################################################

######### DOSE #########  

df_analysis_dose <- df_analysis %>%
  dplyr::filter((study %in% c("DOSE"))) %>%
  mutate(cluster = factor(cluster),
             dummy = rep(1, n())) # Create a dummy variable for the random effect

#** ILINS-DOSE does not have cluster variable **#

mod_gam_dose = gam(laz_el ~ arm_primary + s(wealth_rank, bs = "cr", by = arm_primary),
                     data = df_analysis_dose,
                     family = "gaussian", method = "REML")
summary(mod_gam_dose)

# Get the smooth difference
gam_diff_dose <- get_smooths_difference(mod_gam_dose, wealth_rank, 
            difference = list(arm_primary = c("1","0")))
print(gam_diff_dose)

# Plot the results
gam_plot_dose <- plot_smooths(model = mod_gam_dose, series = wealth_rank, 
                         comparison = arm_primary) +
                 scale_colour_manual(values = c('#E69F00', 'turquoise'),
                                     labels = c("Control", "SQ-LNS")) + 
                 scale_fill_manual(values = c('#E69F00', 'turquoise'),
                                   labels = c("Control", "SQ-LNS")) + 
                 scale_linetype_manual(values=c("solid","dashed"),
                                       labels = c("Control", "SQ-LNS")) +
                 labs(colour = "Arms", fill = "Arms", linetype = "Arms") +
                 labs(x = "Wealth rank based on the wealth index score",
                      y = "Mean LAZ (DOSE)"#, tag = "a \n"
                      ) +
                 coord_cartesian(ylim=c(-2.5,-1.0)) +
                 theme_minimal() +
                 theme(legend.position = c(0.75,0.75),
                       legend.title = element_blank(),
                       legend.key.size = unit(0.3, 'cm'),text = element_text(size=13))

plot_diff_dose <- ggplot(gam_diff_dose, aes(wealth_rank, difference)) +
              geom_hline(aes(yintercept = 0), linetype = "dotted") + 
              geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3) +
              geom_line(aes(), linewidth = 0.6) +
              labs(x = "Wealth rank based on the wealth index score",
              y = "LAZ mean difference (control as ref)"#, tag = "b \n"
              ) +
              theme_minimal() +
              theme(legend.key.size = unit(0.4, 'cm'),text = element_text(size=13))

plot_composite_dose <- ggarrange(gam_plot_dose, plot_diff_dose, 
                               ncol=1, align = "h", labels = c("A", "B"))
plot_composite_dose

# Save the plot
ggsave(here::here("output", "laz_el_wealth_treatment_DOSE.png"), 
       plot = plot_composite_dose, 
       width = 5, height = 8)
       

######### GHANA #########

df_analysis_ghana <- df_analysis %>%
  dplyr::filter((study %in% c("GHANA"))) %>%
  mutate(cluster = factor(cluster),
             dummy = rep(1, n())) # Create a dummy variable for the random effect

#** GHANA does not have cluster variable **#

mod_gam_ghana = gam(laz_el ~ arm_primary + s(wealth_rank, bs = "cr", by = arm_primary),
                     data = df_analysis_ghana,
                     family = "gaussian", method = "REML")
summary(mod_gam_ghana)

# Get the smooth difference
gam_diff_ghana <- get_smooths_difference(mod_gam_ghana, wealth_rank, 
            difference = list(arm_primary = c("1","0")))
print(gam_diff_ghana)

# Plot the results
gam_plot_ghana <- plot_smooths(model = mod_gam_ghana, series = wealth_rank, 
                         comparison = arm_primary) +
                 scale_colour_manual(values = c('#E69F00', 'turquoise'),
                                     labels = c("Control", "SQ-LNS")) + 
                 scale_fill_manual(values = c('#E69F00', 'turquoise'),
                                   labels = c("Control", "SQ-LNS")) + 
                 scale_linetype_manual(values=c("solid","dashed"),
                                       labels = c("Control", "SQ-LNS")) +
                 labs(colour = "Arms", fill = "Arms", linetype = "Arms") +
                 labs(x = "Wealth rank based on the wealth index score",
                      y = "Mean LAZ (GHANA)"#, tag = "a \n"
                      ) +
                 coord_cartesian(ylim=c(-1.5,1.0)) +
                 theme_minimal() +
                 theme(legend.position = c(0.75,0.75),
                       legend.title = element_blank(),
                       legend.key.size = unit(0.3, 'cm'),text = element_text(size=13))

plot_diff_ghana <- ggplot(gam_diff_ghana, aes(wealth_rank, difference)) +
              geom_hline(aes(yintercept = 0), linetype = "dotted") + 
              geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3) +
              geom_line(aes(), linewidth = 0.6) +
              labs(x = "Wealth rank based on the wealth index score",
              y = "LAZ mean difference (control as ref)"#, tag = "b \n"
              ) +
              theme_minimal() +
              theme(legend.key.size = unit(0.4, 'cm'),text = element_text(size=13))

plot_composite_ghana <- ggarrange(gam_plot_ghana, plot_diff_ghana, 
                               ncol=1, align = "h", labels = c("A", "B"))
plot_composite_ghana

# Save the plot
ggsave(here::here("output", "laz_el_wealth_treatment_GHANA.png"), 
       plot = plot_composite_ghana, 
       width = 5, height = 8)


######### HAITI #########

df_analysis_haiti <- df_analysis %>%
  dplyr::filter((study %in% c("HAITI"))) %>%
  mutate(cluster = factor(cluster),
             dummy = rep(1, n())) # Create a dummy variable for the random effect

#** HAITI does not have cluster variable **#

mod_gam_haiti = gam(laz_el ~ arm_primary + s(wealth_rank, bs = "cr", by = arm_primary),
                     data = df_analysis_haiti,
                     family = "gaussian", method = "REML")
summary(mod_gam_haiti)

# Get the smooth difference
gam_diff_haiti <- get_smooths_difference(mod_gam_haiti, wealth_rank, 
            difference = list(arm_primary = c("1","0")))
print(gam_diff_haiti)

# Plot the results
gam_plot_haiti <- plot_smooths(model = mod_gam_haiti, series = wealth_rank, 
                         comparison = arm_primary) +
                 scale_colour_manual(values = c('#E69F00', 'turquoise'),
                                     labels = c("Control", "SQ-LNS")) + 
                 scale_fill_manual(values = c('#E69F00', 'turquoise'),
                                   labels = c("Control", "SQ-LNS")) + 
                 scale_linetype_manual(values=c("solid","dashed"),
                                       labels = c("Control", "SQ-LNS")) +
                 labs(colour = "Arms", fill = "Arms", linetype = "Arms") +
                 labs(x = "Wealth rank based on the wealth index score",
                      y = "Mean LAZ (HAITI)"#, tag = "a \n"
                      ) +
                 coord_cartesian(ylim=c(-1.5,0.0)) +
                 theme_minimal() +
                 theme(legend.position = c(0.75,0.75),
                       legend.title = element_blank(),
                       legend.key.size = unit(0.3, 'cm'),text = element_text(size=13))

plot_diff_haiti <- ggplot(gam_diff_haiti, aes(wealth_rank, difference)) +
              geom_hline(aes(yintercept = 0), linetype = "dotted") + 
              geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3) +
              geom_line(aes(), linewidth = 0.6) +
              labs(x = "Wealth rank based on the wealth index score",
              y = "LAZ mean difference (control as ref)"#, tag = "b \n"
              ) +
              theme_minimal() +
              theme(legend.key.size = unit(0.4, 'cm'),text = element_text(size=13))

plot_composite_haiti <- ggarrange(gam_plot_haiti, plot_diff_haiti, 
                               ncol=1, align = "h", labels = c("A", "B"))
plot_composite_haiti

# Save the plot
ggsave(here::here("output", "laz_el_wealth_treatment_HAITI.png"), 
       plot = plot_composite_haiti, 
       width = 5, height = 8)


######### Studies that were randomized at the cluster level #########
#####################################################################

######### WASHB #########

df_analysis_washb <- df_analysis %>%
  dplyr::filter((study %in% c("WASHB"))) %>%
  mutate(cluster = factor(cluster),
             dummy = rep(1, n())) # Create a dummy variable for the random effect

mod_gam_washb = gam(laz_el ~ arm_primary + s(wealth_rank, bs = "cr", by = arm_primary) +
                     s(cluster, bs = "re", by = dummy), # re is on
                     data = df_analysis_washb,
                     family = "gaussian", method = "REML")
summary(mod_gam_washb)
           
# Get the smooth difference
gam_diff_washb <- get_smooths_difference(mod_gam_washb, wealth_rank, 
            difference = list(arm_primary = c("1","0")))
print(gam_diff_washb)

# Plot the results
gam_plot_washb <- plot_smooths(model = mod_gam_washb, series = wealth_rank, 
                         comparison = arm_primary) +
                 scale_colour_manual(values = c('#E69F00', 'turquoise'),
                                     labels = c("Control", "SQ-LNS")) + 
                 scale_fill_manual(values = c('#E69F00', 'turquoise'),
                                   labels = c("Control", "SQ-LNS")) + 
                 scale_linetype_manual(values=c("solid","dashed"),
                                       labels = c("Control", "SQ-LNS")) +
                 labs(colour = "Arms", fill = "Arms", linetype = "Arms") +
                 labs(x = "Wealth rank based on the wealth index score",
                      y = "Mean LAZ (WASHB)"#, tag = "a \n"
                      ) +
                 coord_cartesian(ylim=c(-2.5,-0.5)) +
                 theme_minimal() +
                 theme(legend.position = c(0.75,0.75),
                       legend.title = element_blank(),
                       legend.key.size = unit(0.3, 'cm'),text = element_text(size=13)) 


plot_diff_washb <- ggplot(gam_diff_washb, aes(wealth_rank, difference)) +
             geom_hline(aes(yintercept = 0), linetype = "dotted") + 
             geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3) +
             geom_line(aes(), linewidth = 0.6) +
             labs(x = "Wealth rank based on the wealth index score",
             y = "LAZ mean difference (control as ref)"#, tag = "b \n"
             ) +
             theme_minimal() +
             theme(legend.key.size = unit(0.4, 'cm'),text = element_text(size=13)) 

plot_composite_washb <- ggarrange(gam_plot_washb, plot_diff_washb, 
                               ncol=1, align = "h", labels = c("A", "B"))
plot_composite_washb

# Save the plot
ggsave(here::here("output", "laz_el_wealth_treatment_WASHB.png"), 
       plot = plot_composite_washb, 
       width = 5, height = 8)


######### JiVitA #########

df_analysis_jivita <- df_analysis %>%
  dplyr::filter((study %in% c("JiVitA"))) %>%
  mutate(cluster = factor(cluster),
             dummy = rep(1, n())) # Create a dummy variable for the random effect

mod_gam_jivita = gam(laz_el ~ arm_primary + s(wealth_rank, bs = "cr", by = arm_primary) +
                     s(cluster, bs = "re", by = dummy), # re is on
                     data = df_analysis_jivita,
                     family = "gaussian", method = "REML")
summary(mod_gam_jivita)

# Get the smooth difference
gam_diff_jivita <- get_smooths_difference(mod_gam_jivita, wealth_rank, 
            difference = list(arm_primary = c("1","0")))
print(gam_diff_jivita)

# Plot the results
gam_plot_jivita <- plot_smooths(model = mod_gam_jivita, series = wealth_rank, 
                         comparison = arm_primary) +
                 scale_colour_manual(values = c('#E69F00', 'turquoise'),
                                     labels = c("Control", "SQ-LNS")) + 
                 scale_fill_manual(values = c('#E69F00', 'turquoise'),
                                   labels = c("Control", "SQ-LNS")) + 
                 scale_linetype_manual(values=c("solid","dashed"),
                                       labels = c("Control", "SQ-LNS")) +
                 labs(colour = "Arms", fill = "Arms", linetype = "Arms") +
                 labs(x = "Wealth rank based on the wealth index score",
                      y = "Mean LAZ (JiVitA)"#, tag = "a \n"
                      ) +
                 coord_cartesian(ylim=c(-2.5,-1.0)) +
                 theme_minimal() +
                 theme(legend.position = c(0.75,0.75),
                       legend.title = element_blank(),
                       legend.key.size = unit(0.3, 'cm'),text = element_text(size=13))

plot_diff_jivita <- ggplot(gam_diff_jivita, aes(wealth_rank, difference)) +
              geom_hline(aes(yintercept = 0), linetype = "dotted") + 
              geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3) +
              geom_line(aes(), linewidth = 0.6) +
              labs(x = "Wealth rank based on the wealth index score",
              y = "LAZ mean difference (control as ref)"#, tag = "b \n"
              ) +
              theme_minimal() +
              theme(legend.key.size = unit(0.4, 'cm'),text = element_text(size=13))

plot_composite_jivita <- ggarrange(gam_plot_jivita, plot_diff_jivita, 
                               ncol=1, align = "h", labels = c("A", "B"))
plot_composite_jivita

# Save the plot
ggsave(here::here("output", "laz_el_wealth_treatment_JiVitA.png"), 
       plot = plot_composite_jivita, 
       width = 5, height = 8)


######### MAHAY #########

df_analysis_mahay <- df_analysis %>%
  dplyr::filter((study %in% c("MAHAY"))) %>%
  mutate(cluster = factor(cluster),
             dummy = rep(1, n())) # Create a dummy variable for the random effect

mod_gam_mahay = gam(laz_el ~ arm_primary + s(wealth_rank, bs = "cr", by = arm_primary) +
                     s(cluster, bs = "re", by = dummy), # re is on
                     data = df_analysis_mahay,
                     family = "gaussian", method = "REML")
summary(mod_gam_mahay)

# Get the smooth difference 
gam_diff_mahay <- get_smooths_difference(mod_gam_mahay, wealth_rank, 
            difference = list(arm_primary = c("1","0")))
print(gam_diff_mahay)

# Plot the results
gam_plot_mahay <- plot_smooths(model = mod_gam_mahay, series = wealth_rank, 
                         comparison = arm_primary) +
                 scale_colour_manual(values = c('#E69F00', 'turquoise'),
                                     labels = c("Control", "SQ-LNS")) + 
                 scale_fill_manual(values = c('#E69F00', 'turquoise'),
                                   labels = c("Control", "SQ-LNS")) + 
                 scale_linetype_manual(values=c("solid","dashed"),
                                       labels = c("Control", "SQ-LNS")) +
                 labs(colour = "Arms", fill = "Arms", linetype = "Arms") +
                 labs(x = "Wealth rank based on the wealth index score",
                      y = "Mean LAZ (MAHAY)"#, tag = "a \n"
                      ) +
                 coord_cartesian(ylim=c(-2.5,-1.8)) +
                 theme_minimal() +
                 theme(legend.position = c(0.75,0.75),
                       legend.title = element_blank(),
                       legend.key.size = unit(0.3, 'cm'),text = element_text(size=13))  

plot_diff_mahay <- ggplot(gam_diff_mahay, aes(wealth_rank, difference)) +
              geom_hline(aes(yintercept = 0), linetype = "dotted") + 
              geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3) +
              geom_line(aes(), linewidth = 0.6) +
              labs(x = "Wealth rank based on the wealth index score",
              y = "LAZ mean difference (control as ref)"#, tag = "b \n"
              ) +
              theme_minimal() +
              theme(legend.key.size = unit(0.4, 'cm'),text = element_text(size=13))

plot_composite_mahay <- ggarrange(gam_plot_mahay, plot_diff_mahay, 
                               ncol=1, align = "h", labels = c("A", "B"))
plot_composite_mahay

# Save the plot
ggsave(here::here("output", "laz_el_wealth_treatment_MAHAY.png"), 
       plot = plot_composite_mahay, 
       width = 5, height = 8)


######### PROMISBF #########

df_analysis_promisbf <- df_analysis %>%
  dplyr::filter((study %in% c("PROMISBF"))) %>%
  mutate(cluster = factor(cluster),
             dummy = rep(1, n())) # Create a dummy variable for the random effect

mod_gam_promisbf = gam(laz_el ~ arm_primary + s(wealth_rank, bs = "cr", by = arm_primary) +
                     s(cluster, bs = "re", by = dummy), # re is on
                     data = df_analysis_promisbf,
                     family = "gaussian", method = "REML")
summary(mod_gam_promisbf)

# Get the smooth difference
gam_diff_promisbf <- get_smooths_difference(mod_gam_promisbf, wealth_rank, 
            difference = list(arm_primary = c("1","0")))
print(gam_diff_promisbf)

# Plot the results
gam_plot_promisbf <- plot_smooths(model = mod_gam_promisbf, series = wealth_rank, 
                         comparison = arm_primary) +
                 scale_colour_manual(values = c('#E69F00', 'turquoise'),
                                     labels = c("Control", "SQ-LNS")) + 
                 scale_fill_manual(values = c('#E69F00', 'turquoise'),
                                   labels = c("Control", "SQ-LNS")) + 
                 scale_linetype_manual(values=c("solid","dashed"),
                                       labels = c("Control", "SQ-LNS")) +
                 labs(colour = "Arms", fill = "Arms", linetype = "Arms") +
                 labs(x = "Wealth rank based on the wealth index score",
                      y = "Mean LAZ (PROMISBF)"#, tag = "a \n"
                      ) +
                 coord_cartesian(ylim=c(-1.7,-1.0)) +
                 theme_minimal() +
                 theme(legend.position = c(0.75,0.90),
                       legend.title = element_blank(),
                       legend.key.size = unit(0.3, 'cm'),text = element_text(size=13))

plot_diff_promisbf <- ggplot(gam_diff_promisbf, aes(wealth_rank, difference)) + 
              geom_hline(aes(yintercept = 0), linetype = "dotted") + 
              geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3) +
              geom_line(aes(), linewidth = 0.6) +
              labs(x = "Wealth rank based on the wealth index score",
              y = "LAZ mean difference (control as ref)"#, tag = "b \n"
              ) +
              theme_minimal() +
              theme(legend.key.size = unit(0.4, 'cm'),text = element_text(size=13))

plot_composite_promisbf <- ggarrange(gam_plot_promisbf, plot_diff_promisbf, 
                               ncol=1, align = "h", labels = c("A", "B"))
plot_composite_promisbf

# Save the plot
ggsave(here::here("output", "laz_el_wealth_treatment_PROMISBF.png"), 
       plot = plot_composite_promisbf, 
       width = 5, height = 8)

######### PROMISBF_CS #########

df_analysis_promisbf_cs <- df_analysis %>%
  dplyr::filter((study %in% c("PROMISBF_CS"))) %>%
  mutate(cluster = factor(cluster),
             dummy = rep(1, n())) # Create a dummy variable for the random effect

mod_gam_promisbf_cs = gam(laz_el ~ arm_primary + s(wealth_rank, bs = "cr", by = arm_primary) +
                     s(cluster, bs = "re", by = dummy), # re is on
                     data = df_analysis_promisbf_cs,
                     family = "gaussian", method = "REML")
summary(mod_gam_promisbf_cs)

# Get the smooth difference
gam_diff_promisbf_cs <- get_smooths_difference(mod_gam_promisbf_cs, wealth_rank, 
            difference = list(arm_primary = c("1","0")))
print(gam_diff_promisbf_cs)

# Plot the results
gam_plot_promisbf_cs <- plot_smooths(model = mod_gam_promisbf_cs, series = wealth_rank, 
                         comparison = arm_primary) +
                 scale_colour_manual(values = c('#E69F00', 'turquoise'),
                                     labels = c("Control", "SQ-LNS")) + 
                 scale_fill_manual(values = c('#E69F00', 'turquoise'),
                                   labels = c("Control", "SQ-LNS")) + 
                 scale_linetype_manual(values=c("solid","dashed"),
                                       labels = c("Control", "SQ-LNS")) +
                 labs(colour = "Arms", fill = "Arms", linetype = "Arms") +
                 labs(x = "Wealth rank based on the wealth index score",
                      y = "Mean LAZ (PROMISBF_CS)"#, tag = "a \n"
                      ) +
                 coord_cartesian(ylim=c(-2.0,-0.5)) +
                 theme_minimal() +
                 theme(legend.position = c(0.75,0.75),
                       legend.title = element_blank(),
                       legend.key.size = unit(0.3, 'cm'),text = element_text(size=13))

plot_diff_promisbf_cs <- ggplot(gam_diff_promisbf_cs, aes(wealth_rank, difference)) +
              geom_hline(aes(yintercept = 0), linetype = "dotted") + 
              geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3) +
              geom_line(aes(), linewidth = 0.6) +
              labs(x = "Wealth rank based on the wealth index score",
              y = "LAZ mean difference (control as ref)"#, tag = "b \n"
              ) +
              theme_minimal() +
              theme(legend.key.size = unit(0.4, 'cm'),text = element_text(size=13))

plot_composite_promisbf_cs <- ggarrange(gam_plot_promisbf_cs, plot_diff_promisbf_cs, 
                               ncol=1, align = "h", labels = c("A", "B"))
plot_composite_promisbf_cs

# Save the plot
ggsave(here::here("output", "laz_el_wealth_treatment_PROMISBF_CS.png"), 
       plot = plot_composite_promisbf_cs, 
       width = 5, height = 8)


######### PROMISM #########

df_analysis_promism <- df_analysis %>%
  dplyr::filter((study %in% c("PROMISM"))) %>%
  mutate(cluster = factor(cluster),
             dummy = rep(1, n())) # Create a dummy variable for the random effect

mod_gam_promism = gam(laz_el ~ arm_primary + s(wealth_rank, bs = "cr", by = arm_primary) +
                     s(cluster, bs = "re", by = dummy), # re is on
                     data = df_analysis_promism,
                     family = "gaussian", method = "REML")
summary(mod_gam_promism)

# Get the smooth difference
gam_diff_promism <- get_smooths_difference(mod_gam_promism, wealth_rank, 
            difference = list(arm_primary = c("1","0")))
print(gam_diff_promism)

# Plot the results
gam_plot_promism <- plot_smooths(model = mod_gam_promism, series = wealth_rank, 
                         comparison = arm_primary) +
                 scale_colour_manual(values = c('#E69F00', 'turquoise'),
                                     labels = c("Control", "SQ-LNS")) + 
                 scale_fill_manual(values = c('#E69F00', 'turquoise'),
                                   labels = c("Control", "SQ-LNS")) + 
                 scale_linetype_manual(values=c("solid","dashed"),
                                       labels = c("Control", "SQ-LNS")) +
                 labs(colour = "Arms", fill = "Arms", linetype = "Arms") +
                 labs(x = "Wealth rank based on the wealth index score",
                      y = "Mean LAZ (PROMISM)"#, tag = "a \n"
                      ) +
                 coord_cartesian(ylim=c(-2.0,-1.0)) +
                 theme_minimal() +
                 theme(legend.position = c(0.75,0.75),
                       legend.title = element_blank(),
                       legend.key.size = unit(0.3, 'cm'),text = element_text(size=13))

plot_diff_promism <- ggplot(gam_diff_promism, aes(wealth_rank, difference)) +
              geom_hline(aes(yintercept = 0), linetype = "dotted") + 
              geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3) +
              geom_line(aes(), linewidth = 0.6) +
              labs(x = "Wealth rank based on the wealth index score",
              y = "LAZ mean difference (control as ref)"#, tag = "b \n"
              ) +
              theme_minimal() +
              theme(legend.key.size = unit(0.4, 'cm'),text = element_text(size=13))

plot_composite_promism <- ggarrange(gam_plot_promism, plot_diff_promism, 
                               ncol=1, align = "h", labels = c("A", "B"))
plot_composite_promism

# Save the plot
ggsave(here::here("output", "laz_el_wealth_treatment_PROMISM.png"), 
       plot = plot_composite_promism, 
       width = 5, height = 8)


######### PROMISM_CS #########

df_analysis_promism_cs <- df_analysis %>%
  dplyr::filter((study %in% c("PROMISM_CS"))) %>%
  mutate(cluster = factor(cluster),
             dummy = rep(1, n())) # Create a dummy variable for the random effect

mod_gam_promism_cs = gam(laz_el ~ arm_primary + s(wealth_rank, bs = "cr", by = arm_primary) +
                     s(cluster, bs = "re", by = dummy), # re is on
                     data = df_analysis_promism_cs,
                     family = "gaussian", method = "REML")
summary(mod_gam_promism_cs)

# Get the smooth difference
gam_diff_promism_cs <- get_smooths_difference(mod_gam_promism_cs, wealth_rank, 
            difference = list(arm_primary = c("1","0")))
print(gam_diff_promism_cs)

# Plot the results
gam_plot_promism_cs <- plot_smooths(model = mod_gam_promism_cs, series = wealth_rank, 
                         comparison = arm_primary) +
                 scale_colour_manual(values = c('#E69F00', 'turquoise'),
                                     labels = c("Control", "SQ-LNS")) + 
                 scale_fill_manual(values = c('#E69F00', 'turquoise'),
                                   labels = c("Control", "SQ-LNS")) + 
                 scale_linetype_manual(values=c("solid","dashed"),
                                       labels = c("Control", "SQ-LNS")) +
                 labs(colour = "Arms", fill = "Arms", linetype = "Arms") +
                 labs(x = "Wealth rank based on the wealth index score",
                      y = "Mean LAZ (PROMISM_CS)"#, tag = "a \n"
                      ) +
                 coord_cartesian(ylim=c(-2.0,-1.0)) +
                 theme_minimal() +
                 theme(legend.position = c(0.75,0.75),
                       legend.title = element_blank(),
                       legend.key.size = unit(0.3, 'cm'),text = element_text(size=13))

plot_diff_promism_cs <- ggplot(gam_diff_promism_cs, aes(wealth_rank, difference)) +
              geom_hline(aes(yintercept = 0), linetype = "dotted") + 
              geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3) +
              geom_line(aes(), linewidth = 0.6) +
              labs(x = "Wealth rank based on the wealth index score",
              y = "LAZ mean difference (control as ref)"#, tag = "b \n"
              ) +
              theme_minimal() +
              theme(legend.key.size = unit(0.4, 'cm'),text = element_text(size=13))

plot_composite_promism_cs <- ggarrange(gam_plot_promism_cs, plot_diff_promism_cs, 
                               ncol=1, align = "h", labels = c("A", "B"))
plot_composite_promism_cs

# Save the plot
ggsave(here::here("output", "laz_el_wealth_treatment_PROMISM_CS.png"), 
       plot = plot_composite_promism_cs, 
       width = 5, height = 8)


######### RDNS #########

df_analysis_rdns <- df_analysis %>%
  dplyr::filter((study %in% c("RDNS"))) %>%
  mutate(cluster = factor(cluster),
             dummy = rep(1, n())) # Create a dummy variable for the random effect

mod_gam_rdns = gam(laz_el ~ arm_primary + s(wealth_rank, bs = "cr", by = arm_primary) +
                     s(cluster, bs = "re", by = dummy), # re is on
                     data = df_analysis_rdns,
                     family = "gaussian", method = "REML")
summary(mod_gam_rdns)

# Get the smooth difference
gam_diff_rdns <- get_smooths_difference(mod_gam_rdns, wealth_rank, 
            difference = list(arm_primary = c("1","0")))
print(gam_diff_rdns)

# Plot the results
gam_plot_rdns <- plot_smooths(model = mod_gam_rdns, series = wealth_rank, 
                         comparison = arm_primary) +
                 scale_colour_manual(values = c('#E69F00', 'turquoise'),
                                     labels = c("Control", "SQ-LNS")) + 
                 scale_fill_manual(values = c('#E69F00', 'turquoise'),
                                   labels = c("Control", "SQ-LNS")) + 
                 scale_linetype_manual(values=c("solid","dashed"),
                                       labels = c("Control", "SQ-LNS")) +
                 labs(colour = "Arms", fill = "Arms", linetype = "Arms") +
                 labs(x = "Wealth rank based on the wealth index score",
                      y = "Mean LAZ (RDNS)"#, tag = "a \n"
                      ) +
                 coord_cartesian(ylim=c(-2.5,-1.0)) +
                 theme_minimal() +
                 theme(legend.position = c(0.75,0.75),
                       legend.title = element_blank(),
                       legend.key.size = unit(0.3, 'cm'),text = element_text(size=13))

plot_diff_rdns <- ggplot(gam_diff_rdns, aes(wealth_rank, difference)) +
              geom_hline(aes(yintercept = 0), linetype = "dotted") + 
              geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3) +
              geom_line(aes(), linewidth = 0.6) +
              labs(x = "Wealth rank based on the wealth index score",
              y = "LAZ mean difference (control as ref)"#, tag = "b \n"
              ) +
              theme_minimal() +
              theme(legend.key.size = unit(0.4, 'cm'),text = element_text(size=13))

plot_composite_rdns <- ggarrange(gam_plot_rdns, plot_diff_rdns, 
                               ncol=1, align = "h", labels = c("A", "B"))
plot_composite_rdns

# Save the plot
ggsave(here::here("output", "laz_el_wealth_treatment_RDNS.png"), 
       plot = plot_composite_rdns, 
       width = 5, height = 8)


######### SHINE_HIV- #########

df_analysis_shine_hiv_neg <- df_analysis %>%
  dplyr::filter((study %in% c("SHINE_HIV-"))) %>%
  mutate(cluster = factor(cluster),
             dummy = rep(1, n())) # Create a dummy variable for the random effect

mod_gam_shine_hiv_neg = gam(laz_el ~ arm_primary + s(wealth_rank, bs = "cr", by = arm_primary) +
                     s(cluster, bs = "re", by = dummy), # re is on
                     data = df_analysis_shine_hiv_neg,
                     family = "gaussian", method = "REML")
summary(mod_gam_shine_hiv_neg)

# Get the smooth difference
gam_diff_shine_hiv_neg <- get_smooths_difference(mod_gam_shine_hiv_neg, wealth_rank, 
            difference = list(arm_primary = c("1","0")))
print(gam_diff_shine_hiv_neg)

# Plot the results
gam_plot_shine_hiv_neg <- plot_smooths(model = mod_gam_shine_hiv_neg, series = wealth_rank, 
                         comparison = arm_primary) +
                 scale_colour_manual(values = c('#E69F00', 'turquoise'),
                                     labels = c("Control", "SQ-LNS")) + 
                 scale_fill_manual(values = c('#E69F00', 'turquoise'),
                                   labels = c("Control", "SQ-LNS")) + 
                 scale_linetype_manual(values=c("solid","dashed"),
                                       labels = c("Control", "SQ-LNS")) +
                 labs(colour = "Arms", fill = "Arms", linetype = "Arms") +
                 labs(x = "Wealth rank based on the wealth index score",
                      y = "Mean LAZ (SHINE_HIV-)"#, tag = "a \n"
                      ) +
                 coord_cartesian(ylim=c(-2.0,-1.0)) +
                 theme_minimal() +
                 theme(legend.position = c(0.75,0.75),
                       legend.title = element_blank(),
                       legend.key.size = unit(0.3, 'cm'),text = element_text(size=13))

plot_diff_shine_hiv_neg <- ggplot(gam_diff_shine_hiv_neg, aes(wealth_rank, difference)) +
              geom_hline(aes(yintercept = 0), linetype = "dotted") + 
              geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3) +
              geom_line(aes(), linewidth = 0.6) +
              labs(x = "Wealth rank based on the wealth index score",
              y = "LAZ mean difference (control as ref)"#, tag = "b \n"
              ) +
              theme_minimal() +
              theme(legend.key.size = unit(0.4, 'cm'),text = element_text(size=13))

plot_composite_shine_hiv_neg <- ggarrange(gam_plot_shine_hiv_neg, plot_diff_shine_hiv_neg, 
                               ncol=1, align = "h", labels = c("A", "B"))
plot_composite_shine_hiv_neg

# Save the plot
ggsave(here::here("output", "laz_el_wealth_treatment_SHINE_HIV-.png"), 
       plot = plot_composite_shine_hiv_neg, 
       width = 5, height = 8)


######### SHINE_HIV+ #########

df_analysis_shine_hiv_pos <- df_analysis %>%
  dplyr::filter((study %in% c("SHINE_HIV+"))) %>%
  mutate(cluster = factor(cluster),
             dummy = rep(1, n())) # Create a dummy variable for the random effect

mod_gam_shine_hiv_pos = gam(laz_el ~ arm_primary + s(wealth_rank, bs = "cr", by = arm_primary) +
                     s(cluster, bs = "re", by = dummy), # re is on
                     data = df_analysis_shine_hiv_pos,
                     family = "gaussian", method = "REML")
summary(mod_gam_shine_hiv_pos)

# Get the smooth difference
gam_diff_shine_hiv_pos <- get_smooths_difference(mod_gam_shine_hiv_pos, wealth_rank, 
            difference = list(arm_primary = c("1","0")))
print(gam_diff_shine_hiv_pos)

# Plot the results
gam_plot_shine_hiv_pos <- plot_smooths(model = mod_gam_shine_hiv_pos, series = wealth_rank, 
                         comparison = arm_primary) +
                 scale_colour_manual(values = c('#E69F00', 'turquoise'),
                                     labels = c("Control", "SQ-LNS")) + 
                 scale_fill_manual(values = c('#E69F00', 'turquoise'),
                                   labels = c("Control", "SQ-LNS")) + 
                 scale_linetype_manual(values=c("solid","dashed"),
                                       labels = c("Control", "SQ-LNS")) +
                 labs(colour = "Arms", fill = "Arms", linetype = "Arms") +
                 labs(x = "Wealth rank based on the wealth index score",
                      y = "Mean LAZ (SHINE_HIV+)"#, tag = "a \n"
                      ) +
                 coord_cartesian(ylim=c(-2.75,-1.25)) +
                 theme_minimal() +
                 theme(legend.position = c(0.75,0.75),
                       legend.title = element_blank(),
                       legend.key.size = unit(0.3, 'cm'),text = element_text(size=13))

plot_diff_shine_hiv_pos <- ggplot(gam_diff_shine_hiv_pos, aes(wealth_rank, difference)) +
              geom_hline(aes(yintercept = 0), linetype = "dotted") + 
              geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3) +
              geom_line(aes(), linewidth = 0.6) +
              labs(x = "Wealth rank based on the wealth index score",
              y = "LAZ mean difference (control as ref)"#, tag = "b \n"
              ) +
              theme_minimal() +
              theme(legend.key.size = unit(0.4, 'cm'),text = element_text(size=13))

plot_composite_shine_hiv_pos <- ggarrange(gam_plot_shine_hiv_pos, plot_diff_shine_hiv_pos, 
                               ncol=1, align = "h", labels = c("A", "B"))
plot_composite_shine_hiv_pos

# Save the plot
ggsave(here::here("output", "laz_el_wealth_treatment_SHINE_HIV+.png"), 
       plot = plot_composite_shine_hiv_pos, 
       width = 5, height = 8)


######### WASHK #########

df_analysis_washk <- df_analysis %>%
  dplyr::filter((study %in% c("WASHK"))) %>%
  mutate(cluster = factor(cluster),
             dummy = rep(1, n())) # Create a dummy variable for the random effect

mod_gam_washk = gam(laz_el ~ arm_primary + s(wealth_rank, bs = "cr", by = arm_primary) +
                     s(cluster, bs = "re", by = dummy), # re is on
                     data = df_analysis_washk,
                     family = "gaussian", method = "REML")
summary(mod_gam_washk)

# Get the smooth difference
gam_diff_washk <- get_smooths_difference(mod_gam_washk, wealth_rank, 
            difference = list(arm_primary = c("1","0")))
print(gam_diff_washk)

# Plot the results
gam_plot_washk <- plot_smooths(model = mod_gam_washk, series = wealth_rank, 
                         comparison = arm_primary) +
                 scale_colour_manual(values = c('#E69F00', 'turquoise'),
                                     labels = c("Control", "SQ-LNS")) + 
                 scale_fill_manual(values = c('#E69F00', 'turquoise'),
                                   labels = c("Control", "SQ-LNS")) + 
                 scale_linetype_manual(values=c("solid","dashed"),
                                       labels = c("Control", "SQ-LNS")) +
                 labs(colour = "Arms", fill = "Arms", linetype = "Arms") +
                 labs(x = "Wealth rank based on the wealth index score",
                      y = "Mean LAZ (WASHK)"#, tag = "a \n"
                      ) +
                 coord_cartesian(ylim=c(-2.0,-1.0)) +
                 theme_minimal() +
                 theme(legend.position = c(0.75,0.75),
                       legend.title = element_blank(),
                       legend.key.size = unit(0.3, 'cm'),text = element_text(size=13))

plot_diff_washk <- ggplot(gam_diff_washk, aes(wealth_rank, difference)) +
              geom_hline(aes(yintercept = 0), linetype = "dotted") + 
              geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3) +
              geom_line(aes(), linewidth = 0.6) +
              labs(x = "Wealth rank based on the wealth index score",
              y = "LAZ mean difference (control as ref)"#, tag = "b \n"
              ) +
              theme_minimal() +
              theme(legend.key.size = unit(0.4, 'cm'),text = element_text(size=13))

plot_composite_washk <- ggarrange(gam_plot_washk, plot_diff_washk, 
                               ncol=1, align = "h", labels = c("A", "B"))
plot_composite_washk

# Save the plot
ggsave(here::here("output", "laz_el_wealth_treatment_WASHK.png"), 
       plot = plot_composite_washk, 
       width = 5, height = 8)


######### ZINC #########

df_analysis_zinc <- df_analysis %>%
  dplyr::filter((study %in% c("ZINC"))) %>%
  mutate(cluster = factor(cluster),
             dummy = rep(1, n())) # Create a dummy variable for the random effect

mod_gam_zinc = gam(laz_el ~ arm_primary + s(wealth_rank, bs = "cr", by = arm_primary) +
                     s(cluster, bs = "re", by = dummy), # re is on
                     data = df_analysis_zinc,
                     family = "gaussian", method = "REML")
summary(mod_gam_zinc)

# Get the smooth difference
gam_diff_zinc <- get_smooths_difference(mod_gam_zinc, wealth_rank, 
            difference = list(arm_primary = c("1","0")))
print(gam_diff_zinc)

# Plot the results
gam_plot_zinc <- plot_smooths(model = mod_gam_zinc, series = wealth_rank, 
                         comparison = arm_primary) +
                 scale_colour_manual(values = c('#E69F00', 'turquoise'),
                                     labels = c("Control", "SQ-LNS")) + 
                 scale_fill_manual(values = c('#E69F00', 'turquoise'),
                                   labels = c("Control", "SQ-LNS")) + 
                 scale_linetype_manual(values=c("solid","dashed"),
                                       labels = c("Control", "SQ-LNS")) +
                 labs(colour = "Arms", fill = "Arms", linetype = "Arms") +
                 labs(x = "Wealth rank based on the wealth index score",
                      y = "Mean LAZ (ZINC)"#, tag = "a \n"
                      ) +
                 coord_cartesian(ylim=c(-2.5,-1.0)) +
                 theme_minimal() +
                 theme(legend.position = c(0.75,0.75),
                       legend.title = element_blank(),
                       legend.key.size = unit(0.3, 'cm'),text = element_text(size=13))

plot_diff_zinc <- ggplot(gam_diff_zinc, aes(wealth_rank, difference)) +
              geom_hline(aes(yintercept = 0), linetype = "dotted") + 
              geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3) +
              geom_line(aes(), linewidth = 0.6) +
              labs(x = "Wealth rank based on the wealth index score",
              y = "LAZ mean difference (control as ref)"#, tag = "b \n"
              ) +
              theme_minimal() +
              theme(legend.key.size = unit(0.4, 'cm'),text = element_text(size=13))

plot_composite_zinc <- ggarrange(gam_plot_zinc, plot_diff_zinc, 
                               ncol=1, align = "h", labels = c("A", "B"))
plot_composite_zinc

# Save the plot
ggsave(here::here("output", "laz_el_wealth_treatment_ZINC.png"), 
       plot = plot_composite_zinc, 
       width = 5, height = 8)



