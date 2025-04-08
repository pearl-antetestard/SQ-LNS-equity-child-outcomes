

rm(list=ls(all=TRUE))

source(here::here("R", "0-config.R"))

##############
##############
### LAZ

render(here::here("R", "gam-meta-analysis-male",
                  "male-pointwise-pooled-laz-arm-maternal.Rmd"))


render(here::here("R", "linear-meta-analysis-male",
                  "male-linear-interaction-laz.Rmd"))

point.wise.absolute_diff_SS.Comb_laz_m <- point.wise.absolute_diff_SS.Comb_laz_m %>% mutate(Model = "GAM")
plot_diff_m <- plot_diff_m %>% mutate(Model = "linear")
point.wise.absolute_diff_m <- point.wise.absolute_diff_m %>% mutate(Model = "linear")

combined_plot_laz_m <- ggplot() +
  geom_line(data = plot_diff_m, aes(x = iwi, y = delta_y_m, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_laz_m, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_laz_m, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
  ylim(c(-0.2, 0.4)) +
  annotate(
    "text", 
    x = min(point.wise.absolute_diff_SS.Comb_laz_m$iwi, na.rm = TRUE) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_laz_m$RE.meta.upper, na.rm = TRUE), 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_m$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_laz_m$iwi, na.rm = TRUE), 
                                  max(point.wise.absolute_diff_SS.Comb_laz_m$iwi, na.rm = TRUE), by = 10)) 


# Print the combined plot
print(combined_plot_laz_m)



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

render(here::here("R", "gam-meta-analysis-male",
                  "male-pointwise-pooled-wlz-arm-maternal.Rmd"))


render(here::here("R", "linear-meta-analysis-male",
                  "male-linear-interaction-wlz.Rmd"))


point.wise.absolute_diff_SS.Comb_wlz_m <- point.wise.absolute_diff_SS.Comb_wlz_m %>% mutate(Model = "GAM")
plot_diff_m <- plot_diff_m %>% mutate(Model = "linear")
point.wise.absolute_diff_m <- point.wise.absolute_diff_m %>% mutate(Model = "linear")

combined_plot_wlz_m <- ggplot() +
  geom_line(data = plot_diff_m, aes(x = iwi, y = delta_y_m, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_wlz_m, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_wlz_m, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
  ylim(c(-0.2, 0.4)) +
  annotate(
    "text", 
    x = min(point.wise.absolute_diff_SS.Comb_wlz_m$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_wlz_m$RE.meta.upper), 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_m$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_wlz_m$iwi), 
                                  max(point.wise.absolute_diff_SS.Comb_wlz_m$iwi), by = 10))

print(combined_plot_wlz_m)


#ggsave(here::here("output", "combined-plot-wlz.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_wlz)


##############
##############
### Stunting


render(here::here("R", "gam-meta-analysis-male",
                  "male-pointwise-pooled-stunted-arm-maternal.Rmd"))


render(here::here("R", "linear-meta-analysis-male",
                  "male-linear-interaction-stunted.Rmd"))

point.wise.absolute_diff_SS.Comb_stunted_m <- point.wise.absolute_diff_SS.Comb_stunted_m %>% mutate(Model = "GAM")
plot_diff_m <- plot_diff_m %>% mutate(Model = "linear")
point.wise.absolute_diff_m <- point.wise.absolute_diff_m %>% mutate(Model = "linear")

combined_plot_stunted_m <- ggplot() +
  geom_line(data = plot_diff_m, aes(x = iwi, y = delta_y_m, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_stunted_m, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_stunted_m, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
  ylim(c(-0.2, 0.4)) +
  annotate(
    "text", 
    x = min(point.wise.absolute_diff_SS.Comb_stunted_m$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_stunted_m$RE.meta.upper) + 0.1, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_m$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_stunted_m$iwi), 
                                  max(point.wise.absolute_diff_SS.Comb_stunted_m$iwi), by = 10))

print(combined_plot_stunted_m)


#ggsave(here::here("output", "combined-plot-stunted.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_stunted)


##############
##############
### Wasting


render(here::here("R", "gam-meta-analysis-male",
                  "male-pointwise-pooled-wasted-arm-maternal.Rmd"))


render(here::here("R", "linear-meta-analysis-male",
                  "male-linear-interaction-wasted.Rmd"))


point.wise.absolute_diff_SS.Comb_wasted_m <- point.wise.absolute_diff_SS.Comb_wasted_m %>% mutate(Model = "GAM")
plot_diff_m <- plot_diff_m %>% mutate(Model = "linear")
point.wise.absolute_diff_m <- point.wise.absolute_diff_m %>% mutate(Model = "linear")

combined_plot_wasted_m <- ggplot() +
  geom_line(data = plot_diff_m, aes(x = iwi, y = delta_y_m, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_wasted_m, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_wasted_m, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
  ylim(c(-0.2, 0.4)) +
  annotate(
    "text", 
    x = min(point.wise.absolute_diff_SS.Comb_wasted_m$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_wasted_m$RE.meta.upper) + 0.1, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_m$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_wasted_m$iwi), 
                                  max(point.wise.absolute_diff_SS.Comb_wasted_m$iwi), by = 10))

print(combined_plot_wasted_m)


#ggsave(here::here("output", "combined-plot-wasted.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_wasted)


##############
##############
### Severe stunting


render(here::here("R", "gam-meta-analysis-male",
                  "male-pointwise-pooled-severe-stunted-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis-male",
                  "male-linear-interaction-severe-stunted.Rmd"))


point.wise.absolute_diff_SS.Comb_severe_stunted_m <- point.wise.absolute_diff_SS.Comb_severe_stunted_m %>% mutate(Model = "GAM")
plot_diff_m <- plot_diff_m %>% mutate(Model = "linear")
point.wise.absolute_diff_m <- point.wise.absolute_diff_m %>% mutate(Model = "linear")

combined_plot_severe_stunted_m <- ggplot() +
  geom_line(data = plot_diff_m, aes(x = iwi, y = delta_y_m, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_severe_stunted_m, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_severe_stunted_m, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
  ylim(c(-0.2, 0.4)) +
  annotate(
    "text", 
    x = min(point.wise.absolute_diff_SS.Comb_severe_stunted_m$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_severe_stunted_m$RE.meta.upper) + 0.1, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_m$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_severe_stunted_m$iwi), 
                                  max(point.wise.absolute_diff_SS.Comb_severe_stunted_m$iwi), by = 10))

print(combined_plot_severe_stunted_m)


#ggsave(here::here("output", "combined-plot-severe-wasted.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_severe_stunted)


##############
##############
### Language score


render(here::here("R", "gam-meta-analysis-male",
                  "male-pointwise-pooled-language-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis-male",
                  "male-linear-interaction-language.Rmd"))


point.wise.absolute_diff_SS.Comb_language_m <- point.wise.absolute_diff_SS.Comb_language_m %>% mutate(Model = "GAM")
plot_diff_m <- plot_diff_m %>% mutate(Model = "linear")
point.wise.absolute_diff_m <- point.wise.absolute_diff_m %>% mutate(Model = "linear")


combined_plot_language_m <- ggplot() +
  geom_line(data = plot_diff_m, aes(x = iwi, y = delta_y_m, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_language_m, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_language_m, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
    x = min(point.wise.absolute_diff_SS.Comb_language_m$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_language_m$RE.meta.upper) + 0.1, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_m$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_language_m$iwi), 
                                  max(point.wise.absolute_diff_SS.Comb_language_m$iwi), by = 10))

print(combined_plot_language_m)

#ggsave(here::here("output", "combined-plot-language.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_language)


##############
##############
### Motor score


render(here::here("R", "gam-meta-analysis-male",
                  "male-pointwise-pooled-gross-motor-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis-male",
                  "male-linear-interaction-gross.Rmd"))


point.wise.absolute_diff_SS.Comb_gross_m <- point.wise.absolute_diff_SS.Comb_gross_m %>% mutate(Model = "GAM")
plot_diff_m <- plot_diff_m %>% mutate(Model = "linear")
point.wise.absolute_diff_m <- point.wise.absolute_diff_m %>% mutate(Model = "linear")


combined_plot_gross_m <- ggplot() +
  geom_line(data = plot_diff_m, aes(x = iwi, y = delta_y_m, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_gross_m, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_gross_m, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
    x = min(point.wise.absolute_diff_SS.Comb_gross_m$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_gross_m$RE.meta.upper) + 0.1, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_m$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_gross_m$iwi), 
                                  max(point.wise.absolute_diff_SS.Comb_gross_m$iwi), by = 10))

print(combined_plot_gross_m)

#ggsave(here::here("output", "combined-plot-gross.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_gross)


##############
##############
### Fine motor score


render(here::here("R", "gam-meta-analysis-male",
                  "male-pointwise-pooled-fine-motor-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis-male",
                  "male-linear-interaction-fine.Rmd"))

point.wise.absolute_diff_SS.Comb_mine_m <- point.wise.absolute_diff_SS.Comb_mine_m %>% mutate(Model = "GAM")
plot_diff_m <- plot_diff_m %>% mutate(Model = "linear")
point.wise.absolute_diff_m <- point.wise.absolute_diff_m %>% mutate(Model = "linear")


combined_plot_mine_m <- ggplot() +
  geom_line(data = plot_diff_m, aes(x = iwi, y = delta_y_m, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_mine_m, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_mine_m, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
    x = min(point.wise.absolute_diff_SS.Comb_mine_m$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_mine_m$RE.meta.upper) + 0.1, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_m$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_mine_m$iwi), 
                                  max(point.wise.absolute_diff_SS.Comb_mine_m$iwi), by = 10))

print(combined_plot_mine_m)

#ggsave(here::here("output", "combined-plot-fine.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_mine)


##############
##############
### Executive function score


render(here::here("R", "gam-meta-analysis-male",
                  "male-pointwise-pooled-executive-function-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis-male",
                  "male-linear-interaction-exec.Rmd"))

point.wise.absolute_diff_SS.Comb_exec_m <- point.wise.absolute_diff_SS.Comb_exec_m %>% mutate(Model = "GAM")
plot_diff_m <- plot_diff_m %>% mutate(Model = "linear")
point.wise.absolute_diff_m <- point.wise.absolute_diff_m %>% mutate(Model = "linear")


combined_plot_exec_m <- ggplot() +
  geom_line(data = plot_diff_m, aes(x = iwi, y = delta_y_m, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_exec_m, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_exec_m, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
    x = min(point.wise.absolute_diff_SS.Comb_exec_m$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_exec_m$RE.meta.upper) + 0.1, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_m$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_exec_m$iwi), 
                                  max(point.wise.absolute_diff_SS.Comb_exec_m$iwi), by = 10))

print(combined_plot_exec_m)

#ggsave(here::here("output", "combined-plot-exec.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_exec)


##############
##############
### Socioemotional score


render(here::here("R", "gam-meta-analysis-male",
                  "male-pointwise-pooled-socioemotional-score-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis-male",
                  "male-linear-interaction-socioemo.Rmd"))

point.wise.absolute_diff_SS.Comb_socioemo_m <- point.wise.absolute_diff_SS.Comb_socioemo_m %>% mutate(Model = "GAM")
plot_diff_m <- plot_diff_m %>% mutate(Model = "linear")
point.wise.absolute_diff_m <- point.wise.absolute_diff_m %>% mutate(Model = "linear")


combined_plot_socioemo_m <- ggplot() +
  geom_line(data = plot_diff_m, aes(x = iwi, y = delta_y_m, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_socioemo_m, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_socioemo_m, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
    x = min(point.wise.absolute_diff_SS.Comb_socioemo_m$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_socioemo_m$RE.meta.upper), 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_m$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_socioemo_m$iwi), 
                                  max(point.wise.absolute_diff_SS.Comb_socioemo_m$iwi), by = 10))

print(combined_plot_socioemo_m)

#ggsave(here::here("output", "combined-plot-socioemo.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_socioemo)


##############
##############
### Hemoglobin concentration


render(here::here("R", "gam-meta-analysis-male",
                  "male-pointwise-pooled-hemoglobin-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis-male",
                  "male-linear-interaction-hgb.Rmd"))

point.wise.absolute_diff_SS.Comb_hgb_m <- point.wise.absolute_diff_SS.Comb_hgb_m %>% mutate(Model = "GAM")
plot_diff_m <- plot_diff_m %>% mutate(Model = "linear")
point.wise.absolute_diff_m <- point.wise.absolute_diff_m %>% mutate(Model = "linear")


combined_plot_hgb_m <- ggplot() +
  geom_line(data = plot_diff_m, aes(x = iwi, y = delta_y_m, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_hgb_m, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_hgb_m, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
    x = min(point.wise.absolute_diff_SS.Comb_hgb_m$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_hgb_m$RE.meta.upper) + 0.6, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_m$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_hgb_m$iwi), 
                                  max(point.wise.absolute_diff_SS.Comb_hgb_m$iwi), by = 10))


print(combined_plot_hgb_m)

#ggsave(here::here("output", "combined-plot-hgb.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_hgb)


##############
##############
### Anemia


render(here::here("R", "gam-meta-analysis-male",
                  "male-pointwise-pooled-anemia-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis-male",
                  "male-linear-interaction-anemia.Rmd"))

point.wise.absolute_diff_SS.Comb_anemia_m <- point.wise.absolute_diff_SS.Comb_anemia_m %>% mutate(Model = "GAM")
plot_diff_m <- plot_diff_m %>% mutate(Model = "linear")
point.wise.absolute_diff_m <- point.wise.absolute_diff_m %>% mutate(Model = "linear")


combined_plot_anemia_m <- ggplot() +
  geom_line(data = plot_diff_m, aes(x = iwi, y = delta_y_m, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_anemia_m, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_anemia_m, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
    x = min(point.wise.absolute_diff_SS.Comb_anemia_m$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_anemia_m$RE.meta.upper) + 0.04, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide_m$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_anemia_m$iwi), 
                                  max(point.wise.absolute_diff_SS.Comb_anemia_m$iwi), by = 10))

print(combined_plot_anemia_m)

#ggsave(here::here("output", "combined-plot-anemia.png"), width = 8,
#      height = 6, dpi = 300, combined_plot_anemia)

