
## Pooled RII and SII - binary outcomes

rm(list=ls(all=TRUE))


source(here::here("R", "0-config.R"))

render(here::here("R", "rii-sii-ineq",
                  "rii-sii-ineq-stunted.Rmd"))
render(here::here("R", "rii-sii-ineq",
                  "rii-sii-ineq-wasted.Rmd"))
render(here::here("R", "rii-sii-ineq",
                  "rii-sii-ineq-severe-stunted.Rmd"))
render(here::here("R", "rii-sii-ineq",
                  "rii-sii-ineq-anemia.Rmd"))


## RII

# Add outcome column
meta_results_rii_wasted$outcome <- "Wasting"
meta_results_rii_stunted$outcome <- "Stunting"
meta_results_rii_severe_stunted$outcome <- "Severe stunting"
meta_results_rii_anemia$outcome <- "Anemia"


# Combine data frames
combined_data_rii <- rbind(meta_results_rii_stunted, meta_results_rii_wasted,
                       meta_results_rii_severe_stunted, meta_results_rii_anemia)

# Plot
#ggplot(combined_data, aes(x = arm, y = RE.meta, color = outcome)) +
  #geom_point(position = position_dodge(width = 0.2)) +
  #geom_errorbar(aes(ymin = RE.meta.lower, ymax = RE.meta.upper), 
  #              width = 0.1, position = position_dodge(width = 0.2)) +
  #facet_wrap(~ outcome, scales = "free_x")

# Create a combined factor for the x-axis
combined_data_rii$arm_outcome <- interaction(combined_data_rii$arm, combined_data_rii$outcome, sep = " - ")
combined_data_rii$arm_outcome <- factor(combined_data_rii$arm_outcome, levels = c(
  "Control - Wasting", "SQ-LNS - Wasting",
  "Control - Stunting", "SQ-LNS - Stunting",
  "Control - Severe stunting", "SQ-LNS - Severe stunting",
  "Control - Anemia", "SQ-LNS - Anemia"
))

# Plot
plot1 <- 
  ggplot(combined_data_rii, aes(x = arm_outcome, y = RE.meta)) +
  geom_errorbar(aes(ymin = RE.meta.lower, ymax = RE.meta.upper, color = outcome), 
                width = 0.1, position = position_dodge(width = 0.2)) +
  geom_point(aes(color = outcome, shape = arm_outcome), position = position_dodge(width = 0.2), size = 3) +
  labs(x = "Intervention Arms", y = "Pooled RII (95% CI)", 
       color = "Child outcome", tag = "A") +
  theme_minimal() +
  geom_hline(yintercept = 1, linetype = "dotted") +
  scale_color_manual(values = c("Wasting" = "orange", "Stunting" = "darkblue", "Severe stunting" = "darkgreen", "Anemia" = "purple")) +
  scale_shape_manual(values = c(
    "Control - Wasting" = 1,
    "SQ-LNS - Wasting" = 16,
    "Control - Stunting" = 1,
    "SQ-LNS - Stunting" = 16,
    "Control - Severe stunting" = 1,
    "SQ-LNS - Severe stunting" = 16,
    "Control - Anemia" = 1,
    "SQ-LNS - Anemia" = 16
  )) +
  scale_x_discrete(labels = c(
    "Control - Wasting" = "Control",
    "SQ-LNS - Wasting" = "SQ-LNS",
    "Control - Stunting" = "Control",
    "SQ-LNS - Stunting" = "SQ-LNS",
    "Control - Severe stunting" = "Control",
    "SQ-LNS - Severe stunting" = "SQ-LNS",
    "Control - Anemia" = "Control",
    "SQ-LNS - Anemia" = "SQ-LNS"
  )) +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "none") +
  annotate("text", x = 1.0, y = 2.5, label = "Wasting", hjust = 0, vjust = 1, 
    size = 4, color = "orange") +
  annotate("text", x = 3.0, y = 2.0, label = "Stunting", hjust = 0, vjust = 1,
    size = 4, color = "darkblue") +
  annotate("text", x = 4.7, y = 3.1, label = "Severe stunting", hjust = 0, vjust = 1,
    size = 4, color = "darkgreen") +
  annotate("text", x = 7.0, y = 1.3, label = "Anemia", hjust = 0, vjust = 1,
    size = 4, color = "purple") +
  annotate("point", x = 6.8, y = 3.1, shape = 1, size = 3, color = "black") +
  annotate("text", x = 7, y = 3.1, label = "Control", hjust = 0, vjust = 0.5, size = 4) +
  annotate("point", x = 6.8, y = 2.9, shape = 16, size = 3, color = "black") +
  annotate("text", x = 7, y = 2.9, label = "SQ-LNS", hjust = 0, vjust = 0.5, size = 4)

plot1


## SII

meta_results_sii_wasted$outcome <- "Wasting"
meta_results_sii_stunted$outcome <- "Stunting"
meta_results_sii_severe_stunted$outcome <- "Severe stunting"
meta_results_sii_anemia$outcome <- "Anemia"

combined_data_sii <- rbind(meta_results_sii_stunted, meta_results_sii_wasted,
                       meta_results_sii_severe_stunted, meta_results_sii_anemia)

# Create a combined factor for the x-axis
combined_data_sii$arm_outcome <- interaction(combined_data_sii$arm, combined_data_sii$outcome, sep = " - ")
combined_data_sii$arm_outcome <- factor(combined_data_sii$arm_outcome, levels = c(
  "Control - Wasting", "SQ-LNS - Wasting",
  "Control - Stunting", "SQ-LNS - Stunting",
  "Control - Severe stunting", "SQ-LNS - Severe stunting",
  "Control - Anemia", "SQ-LNS - Anemia"
))

# Plot
plot2 <- ggplot(combined_data_sii, aes(x = arm_outcome, y = RE.meta * 100)) +
  geom_errorbar(aes(ymin = RE.meta.lower * 100, ymax = RE.meta.upper * 100, color = outcome), 
                width = 0.1, position = position_dodge(width = 0.2)) +
  geom_point(aes(color = outcome, shape = arm_outcome), position = position_dodge(width = 0.2), size = 3) +
  labs(x = "Intervention Arms", y = "Pooled SII (95% CI)", 
       color = "Child outcome", tag = "B") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_color_manual(values = c("Wasting" = "orange", "Stunting" = "darkblue", "Severe stunting" = "darkgreen", "Anemia" = "purple")) +
  scale_shape_manual(values = c(
    "Control - Wasting" = 1,
    "SQ-LNS - Wasting" = 16,
    "Control - Stunting" = 1,
    "SQ-LNS - Stunting" = 16,
    "Control - Severe stunting" = 1,
    "SQ-LNS - Severe stunting" = 16,
    "Control - Anemia" = 1,
    "SQ-LNS - Anemia" = 16
  )) +
  scale_x_discrete(labels = c(
    "Control - Wasting" = "Control",
    "SQ-LNS - Wasting" = "SQ-LNS",
    "Control - Stunting" = "Control",
    "SQ-LNS - Stunting" = "SQ-LNS",
    "Control - Severe stunting" = "Control",
    "SQ-LNS - Severe stunting" = "SQ-LNS",
    "Control - Anemia" = "Control",
    "SQ-LNS - Anemia" = "SQ-LNS"
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  annotate("text", x = 1.0, y = 7.8, label = "Wasting", hjust = 0, vjust = 1, 
           size = 4, color = "orange") +
  annotate("text", x = 3.0, y = 20.5, label = "Stunting", hjust = 0, vjust = 1,
           size = 4, color = "darkblue") +
  annotate("text", x = 4.7, y = 12, label = "Severe stunting", hjust = 0, vjust = 1,
           size = 4, color = "darkgreen") +
  annotate("text", x = 7.0, y = 8.7, label = "Anemia", hjust = 0, vjust = 1,
           size = 4, color = "purple") 


plot2

# Combine the plots with a common x-axis
combined_plot <- plot1 / plot2

# Display the combined plot
combined_plot


ggsave(here::here("output", "rii-sii-ineq-revised.png"), combined_plot, width = 7, height = 7, dpi = 300)
