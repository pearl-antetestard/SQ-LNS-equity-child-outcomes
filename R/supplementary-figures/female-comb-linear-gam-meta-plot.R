## Author: Pearl Ante-Testard
## email: pearl.ante@ucsf.edu

rm(list=ls(all=TRUE))

source(here::here("R", "0-config.R"))

##############
##############
### LAZ

render(here::here("R", "gam-meta-analysis-female",
                  "female-pointwise-pooled-laz-arm-maternal.Rmd"))


render(here::here("R", "linear-meta-analysis-female",
                  "female-linear-interaction-laz.Rmd"))

point.wise.absolute_diff_SS.Comb_laz_f <- point.wise.absolute_diff_SS.Comb_laz_f %>% mutate(Model = "GAM")
plot_diff_f <- plot_diff_f %>% mutate(Model = "linear")
point.wise.absolute_diff_f <- point.wise.absolute_diff_f %>% mutate(Model = "linear")

combined_plot_laz_f <- ggplot() +
  geom_line(data = plot_diff_f, aes(x = iwi, y = delta_y_f, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_laz_f, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_laz_f, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
  geom_hline(yintercept = 0, linetype = 2) +
  xlab(bquote('IWI')) +
  ylab(bquote('Absolute difference')) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    strip.text = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 12)
  ) +
  ylim(c(-0.15, 0.4)) +
  annotate(
    "text", 
    x = min(point.wise.absolute_diff_SS.Comb_laz_f$iwi, na.rm = TRUE) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_laz_f$RE.meta.upper, na.rm = TRUE), 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_f$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_laz_f$iwi, na.rm = TRUE), 
                                  max(point.wise.absolute_diff_SS.Comb_laz_f$iwi, na.rm = TRUE), by = 10)) 


# Print the combined plot
print(combined_plot_laz_f)



#ggsave(here::here("output", "combined-plot-laz-1.png"), width = 8, 
#       height = 6, dpi = 300, combined_plot_1)
#ggsave(here::here("output", "combined-plot-laz-2.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_2)
#ggsave(here::here("output", "combined-plot-laz-3.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_3)
#ggsave(here::here("output", "combined-plot-laz.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_laz)


##############
##############
### WLZ

render(here::here("R", "gam-meta-analysis-female",
                  "female-pointwise-pooled-wlz-arm-maternal.Rmd"))


render(here::here("R", "linear-meta-analysis-female",
                  "female-linear-interaction-wlz.Rmd"))


point.wise.absolute_diff_SS.Comb_wlz_f <- point.wise.absolute_diff_SS.Comb_wlz_f %>% mutate(Model = "GAM")
plot_diff_f <- plot_diff_f %>% mutate(Model = "linear")
point.wise.absolute_diff_f <- point.wise.absolute_diff_f %>% mutate(Model = "linear")

combined_plot_wlz_f <- ggplot() +
  geom_line(data = plot_diff_f, aes(x = iwi, y = delta_y_f, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_wlz_f, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_wlz_f, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
  geom_hline(yintercept = 0, linetype = 2) +
  xlab(bquote('IWI')) +
  ylab(bquote('Absolute difference')) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    strip.text = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 12)
  ) +
  ylim(c(-0.15, 0.4)) +
  annotate(
    "text", 
    x = min(point.wise.absolute_diff_SS.Comb_wlz_f$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_wlz_f$RE.meta.upper), 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_f$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_wlz_f$iwi), 
                                  max(point.wise.absolute_diff_SS.Comb_wlz_f$iwi), by = 10))

print(combined_plot_wlz_f)


#ggsave(here::here("output", "combined-plot-wlz.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_wlz)


##############
##############
### Stunting


render(here::here("R", "gam-meta-analysis-female",
                  "female-pointwise-pooled-stunted-arm-maternal.Rmd"))


render(here::here("R", "linear-meta-analysis-female",
                  "female-linear-interaction-stunted.Rmd"))

point.wise.absolute_diff_SS.Comb_stunted_f <- point.wise.absolute_diff_SS.Comb_stunted_f %>% mutate(Model = "GAM")
plot_diff_f <- plot_diff_f %>% mutate(Model = "linear")
point.wise.absolute_diff_f <- point.wise.absolute_diff_f %>% mutate(Model = "linear")

combined_plot_stunted_f <- ggplot() +
  geom_line(data = plot_diff_f, aes(x = iwi, y = delta_y_f, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_stunted_f, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_stunted_f, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
  geom_hline(yintercept = 0, linetype = 2) +
  xlab(bquote('IWI')) +
  ylab(bquote('Absolute difference')) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    strip.text = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 12)
  ) +
  ylim(c(-0.15, 0.4)) +
  annotate(
    "text", 
    x = min(point.wise.absolute_diff_SS.Comb_stunted_f$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_stunted_f$RE.meta.upper) + 0.1, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_f$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_stunted_f$iwi), 
                                  max(point.wise.absolute_diff_SS.Comb_stunted_f$iwi), by = 10))

print(combined_plot_stunted_f)


#ggsave(here::here("output", "combined-plot-stunted.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_stunted)


##############
##############
### Wasting


render(here::here("R", "gam-meta-analysis-female",
                  "female-pointwise-pooled-wasted-arm-maternal.Rmd"))


render(here::here("R", "linear-meta-analysis-female",
                  "female-linear-interaction-wasted.Rmd"))


point.wise.absolute_diff_SS.Comb_wasted_f <- point.wise.absolute_diff_SS.Comb_wasted_f %>% mutate(Model = "GAM")
plot_diff_f <- plot_diff_f %>% mutate(Model = "linear")
point.wise.absolute_diff_f <- point.wise.absolute_diff_f %>% mutate(Model = "linear")

combined_plot_wasted_f <- ggplot() +
  geom_line(data = plot_diff_f, aes(x = iwi, y = delta_y_f, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_wasted_f, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_wasted_f, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
  geom_hline(yintercept = 0, linetype = 2) +
  xlab(bquote('IWI')) +
  ylab(bquote('Absolute difference')) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    strip.text = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 12)
  ) +
  ylim(c(-0.15, 0.4)) +
  annotate(
    "text", 
    x = min(point.wise.absolute_diff_SS.Comb_wasted_f$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_wasted_f$RE.meta.upper) + 0.1, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_f$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_wasted_f$iwi), 
                                  max(point.wise.absolute_diff_SS.Comb_wasted_f$iwi), by = 10))

print(combined_plot_wasted_f)


#ggsave(here::here("output", "combined-plot-wasted.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_wasted)


##############
##############
### Severe stunting


render(here::here("R", "gam-meta-analysis-female",
                  "female-pointwise-pooled-severe-stunted-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis-female",
                  "female-linear-interaction-severe-stunted.Rmd"))


point.wise.absolute_diff_SS.Comb_severe_stunted_f <- point.wise.absolute_diff_SS.Comb_severe_stunted_f %>% mutate(Model = "GAM")
plot_diff_f <- plot_diff_f %>% mutate(Model = "linear")
point.wise.absolute_diff_f <- point.wise.absolute_diff_f %>% mutate(Model = "linear")

combined_plot_severe_stunted_f <- ggplot() +
  geom_line(data = plot_diff_f, aes(x = iwi, y = delta_y_f, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_severe_stunted_f, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_severe_stunted_f, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
  geom_hline(yintercept = 0, linetype = 2) +
  xlab(bquote('IWI')) +
  ylab(bquote('Absolute difference')) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    strip.text = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 12)
  ) +
  ylim(c(-0.15, 0.4)) +
  annotate(
    "text", 
    x = min(point.wise.absolute_diff_SS.Comb_severe_stunted_f$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_severe_stunted_f$RE.meta.upper) + 0.1, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_f$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_severe_stunted_f$iwi), 
                                  max(point.wise.absolute_diff_SS.Comb_severe_stunted_f$iwi), by = 10))

print(combined_plot_severe_stunted_f)


#ggsave(here::here("output", "combined-plot-severe-wasted.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_severe_stunted)


##############
##############
### Language score


render(here::here("R", "gam-meta-analysis-female",
                  "female-pointwise-pooled-language-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis-female",
                  "female-linear-interaction-language.Rmd"))


point.wise.absolute_diff_SS.Comb_language_f <- point.wise.absolute_diff_SS.Comb_language_f %>% mutate(Model = "GAM")
plot_diff_f <- plot_diff_f %>% mutate(Model = "linear")
point.wise.absolute_diff_f <- point.wise.absolute_diff_f %>% mutate(Model = "linear")


combined_plot_language_f <- ggplot() +
  geom_line(data = plot_diff_f, aes(x = iwi, y = delta_y_f, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_language_f, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_language_f, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
  geom_hline(yintercept = 0, linetype = 2) +
  xlab(bquote('IWI')) +
  ylab(bquote('Absolute difference')) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    strip.text = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 12)
  ) +
  ylim(c(-0.25, 0.5)) +
  annotate(
    "text", 
    x = min(point.wise.absolute_diff_SS.Comb_language_f$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_language_f$RE.meta.upper) + 0.1, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_f$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_language_f$iwi), 
                                  max(point.wise.absolute_diff_SS.Comb_language_f$iwi), by = 10))

print(combined_plot_language_f)

#ggsave(here::here("output", "combined-plot-language.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_language)


##############
##############
### Motor score


render(here::here("R", "gam-meta-analysis-female",
                  "female-pointwise-pooled-gross-motor-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis-female",
                  "female-linear-interaction-gross.Rmd"))


point.wise.absolute_diff_SS.Comb_gross_f <- point.wise.absolute_diff_SS.Comb_gross_f %>% mutate(Model = "GAM")
plot_diff_f <- plot_diff_f %>% mutate(Model = "linear")
point.wise.absolute_diff_f <- point.wise.absolute_diff_f %>% mutate(Model = "linear")


combined_plot_gross_f <- ggplot() +
  geom_line(data = plot_diff_f, aes(x = iwi, y = delta_y_f, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_gross_f, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_gross_f, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
  geom_hline(yintercept = 0, linetype = 2) +
  xlab(bquote('IWI')) +
  ylab(bquote('Absolute difference')) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    strip.text = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 12)
  ) +
  ylim(c(-0.25, 0.5)) +
  annotate(
    "text", 
    x = min(point.wise.absolute_diff_SS.Comb_gross_f$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_gross_f$RE.meta.upper) + 0.1, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_f$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_gross_f$iwi), 
                                  max(point.wise.absolute_diff_SS.Comb_gross_f$iwi), by = 10))

print(combined_plot_gross_f)

#ggsave(here::here("output", "combined-plot-gross.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_gross)


##############
##############
### Fine motor score


render(here::here("R", "gam-meta-analysis-female",
                  "female-pointwise-pooled-fine-motor-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis-female",
                  "female-linear-interaction-fine.Rmd"))

point.wise.absolute_diff_SS.Comb_fine_f <- point.wise.absolute_diff_SS.Comb_fine_f %>% mutate(Model = "GAM")
plot_diff_f <- plot_diff_f %>% mutate(Model = "linear")
point.wise.absolute_diff_f <- point.wise.absolute_diff_f %>% mutate(Model = "linear")


combined_plot_fine_f <- ggplot() +
  geom_line(data = plot_diff_f, aes(x = iwi, y = delta_y_f, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_fine_f, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_fine_f, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
  geom_hline(yintercept = 0, linetype = 2) +
  xlab(bquote('IWI')) +
  ylab(bquote('Absolute difference')) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    strip.text = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 12)
  ) +
  ylim(c(-0.25, 0.5)) +
  annotate(
    "text", 
    x = min(point.wise.absolute_diff_SS.Comb_fine_f$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_fine_f$RE.meta.upper), 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_f$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_fine_f$iwi), 
                                  max(point.wise.absolute_diff_SS.Comb_fine_f$iwi), by = 10))

print(combined_plot_fine_f)

#ggsave(here::here("output", "combined-plot-fine.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_fine)


##############
##############
### Executive function score


render(here::here("R", "gam-meta-analysis-female",
                  "female-pointwise-pooled-executive-function-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis-female",
                  "female-linear-interaction-exec.Rmd"))

point.wise.absolute_diff_SS.Comb_exec_f <- point.wise.absolute_diff_SS.Comb_exec_f %>% mutate(Model = "GAM")
plot_diff_f <- plot_diff_f %>% mutate(Model = "linear")
point.wise.absolute_diff_f <- point.wise.absolute_diff_f %>% mutate(Model = "linear")


combined_plot_exec_f <- ggplot() +
  geom_line(data = plot_diff_f, aes(x = iwi, y = delta_y_f, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_exec_f, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_exec_f, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
  geom_hline(yintercept = 0, linetype = 2) +
  xlab(bquote('IWI')) +
  ylab(bquote('Absolute difference')) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    strip.text = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 12)
  ) +
  ylim(c(-0.25, 0.5)) +
  annotate(
    "text", 
    x = min(point.wise.absolute_diff_SS.Comb_exec_f$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_exec_f$RE.meta.upper) + 0.1, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_f$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_exec_f$iwi), 
                                  max(point.wise.absolute_diff_SS.Comb_exec_f$iwi), by = 10))

print(combined_plot_exec_f)

#ggsave(here::here("output", "combined-plot-exec.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_exec)


##############
##############
### Socioemotional score


render(here::here("R", "gam-meta-analysis-female",
                  "female-pointwise-pooled-socioemotional-score-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis-female",
                  "female-linear-interaction-socioemo.Rmd"))

point.wise.absolute_diff_SS.Comb_socioemo_f <- point.wise.absolute_diff_SS.Comb_socioemo_f %>% mutate(Model = "GAM")
plot_diff_f <- plot_diff_f %>% mutate(Model = "linear")
point.wise.absolute_diff_f <- point.wise.absolute_diff_f %>% mutate(Model = "linear")


combined_plot_socioemo_f <- ggplot() +
  geom_line(data = plot_diff_f, aes(x = iwi, y = delta_y_f, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_socioemo_f, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_socioemo_f, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
  geom_hline(yintercept = 0, linetype = 2) +
  xlab(bquote('IWI')) +
  ylab(bquote('Absolute difference')) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    strip.text = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 12)
  ) +
  ylim(c(-0.25, 0.5)) +
  annotate(
    "text", 
    x = min(point.wise.absolute_diff_SS.Comb_socioemo_f$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_socioemo_f$RE.meta.upper) + 0.04, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_f$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_socioemo_f$iwi), 
                                  max(point.wise.absolute_diff_SS.Comb_socioemo_f$iwi), by = 10))

print(combined_plot_socioemo_f)

#ggsave(here::here("output", "combined-plot-socioemo.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_socioemo)


##############
##############
### Hemoglobin concentration


render(here::here("R", "gam-meta-analysis-female",
                  "female-pointwise-pooled-hemoglobin-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis-female",
                  "female-linear-interaction-hgb.Rmd"))

point.wise.absolute_diff_SS.Comb_hgb_f <- point.wise.absolute_diff_SS.Comb_hgb_f %>% mutate(Model = "GAM")
plot_diff_f <- plot_diff_f %>% mutate(Model = "linear")
point.wise.absolute_diff_f <- point.wise.absolute_diff_f %>% mutate(Model = "linear")


combined_plot_hgb_f <- ggplot() +
  geom_line(data = plot_diff_f, aes(x = iwi, y = delta_y_f, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_hgb_f, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_hgb_f, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
  geom_hline(yintercept = 0, linetype = 2) +
  xlab(bquote('IWI')) +
  ylab(bquote('Absolute difference')) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    strip.text = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 12)
  ) +
  ylim(c(-2, 10)) +
  annotate(
    "text", 
    x = min(point.wise.absolute_diff_SS.Comb_hgb_f$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_hgb_f$RE.meta.upper) + 0.6, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_f$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_hgb_f$iwi), 
                                  max(point.wise.absolute_diff_SS.Comb_hgb_f$iwi), by = 10))


print(combined_plot_hgb_f)

#ggsave(here::here("output", "combined-plot-hgb.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_hgb)


##############
##############
### Anemia


render(here::here("R", "gam-meta-analysis-female",
                  "female-pointwise-pooled-anemia-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis-female",
                  "female-linear-interaction-anemia.Rmd"))

point.wise.absolute_diff_SS.Comb_anemia_f <- point.wise.absolute_diff_SS.Comb_anemia_f %>% mutate(Model = "GAM")
plot_diff_f <- plot_diff_f %>% mutate(Model = "linear")
point.wise.absolute_diff_f <- point.wise.absolute_diff_f %>% mutate(Model = "linear")


combined_plot_anemia_f <- ggplot() +
  geom_line(data = plot_diff_f, aes(x = iwi, y = delta_y_f, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_anemia_f, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_anemia_f, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
  geom_hline(yintercept = 0, linetype = 2) +
  xlab(bquote('IWI')) +
  ylab(bquote('Absolute difference')) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    strip.text = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 12)
  ) +
  ylim(c(-0.3, 0.2)) +
  annotate(
    "text", 
    x = min(point.wise.absolute_diff_SS.Comb_anemia_f$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_anemia_f$RE.meta.upper) + 0.04, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_f$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_anemia_f$iwi), 
                                  max(point.wise.absolute_diff_SS.Comb_anemia_f$iwi), by = 10))

print(combined_plot_anemia_f)

#ggsave(here::here("output", "combined-plot-anemia.png"), width = 8,
#      height = 6, dpi = 300, combined_plot_anemia)

