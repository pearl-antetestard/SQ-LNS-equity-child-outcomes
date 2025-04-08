## Author: Pearl Ante-Testard
## email: pearl.ante@ucsf.edu

#rm(list=ls(all=TRUE))

source(here::here("R", "0-config.R"))

render(here::here("R", "gam-meta-analysis",
                  "pointwise-pooled-laz-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis",
                  "linear-interaction-laz.Rmd"))


##############
##############
### LAZ

point.wise.absolute_diff_SS.Comb_laz <- point.wise.absolute_diff_SS.Comb_laz %>% mutate(Model = "GAM")
plot_diff <- plot_diff %>% mutate(Model = "linear")
point.wise.absolute_diff <- point.wise.absolute_diff %>% mutate(Model = "linear")

combined_plot_laz <- ggplot() +
  geom_line(data = plot_diff, aes(x = iwi, y = delta_y, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_laz, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_laz, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
    x = min(point.wise.absolute_diff_SS.Comb_laz$iwi, na.rm = TRUE) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_laz$RE.meta.upper, na.rm = TRUE), 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_laz$iwi, na.rm = TRUE), max(point.wise.absolute_diff_SS.Comb_laz$iwi, na.rm = TRUE), by = 10)) 

               
# Print the combined plot
print(combined_plot_laz)
# Print the combined plot



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



render(here::here("R", "gam-meta-analysis",
                  "pointwise-pooled-wlz-arm-maternal.Rmd"))


render(here::here("R", "linear-meta-analysis",
                  "linear-interaction-wlz.Rmd"))


point.wise.absolute_diff_SS.Comb_wlz <- point.wise.absolute_diff_SS.Comb_wlz %>% mutate(Model = "GAM")
plot_diff <- plot_diff %>% mutate(Model = "linear")
point.wise.absolute_diff <- point.wise.absolute_diff %>% mutate(Model = "linear")

combined_plot_wlz <- ggplot() +
  geom_line(data = plot_diff, aes(x = iwi, y = delta_y, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_wlz, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_wlz, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
    x = min(point.wise.absolute_diff_SS.Comb_wlz$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_wlz$RE.meta.upper), 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_wlz$iwi), max(point.wise.absolute_diff_SS.Comb_wlz$iwi), by = 10))

print(combined_plot_wlz)


#ggsave(here::here("output", "combined-plot-wlz.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_wlz)


##############
##############
### Stunting


render(here::here("R", "gam-meta-analysis",
                  "pointwise-pooled-stunted-arm-maternal.Rmd"))


render(here::here("R", "linear-meta-analysis",
                  "linear-interaction-stunted.Rmd"))

point.wise.absolute_diff_SS.Comb_stunted <- point.wise.absolute_diff_SS.Comb_stunted %>% mutate(Model = "GAM")
plot_diff <- plot_diff %>% mutate(Model = "linear")
point.wise.absolute_diff <- point.wise.absolute_diff %>% mutate(Model = "linear")

combined_plot_stunted <- ggplot() +
  geom_line(data = plot_diff, aes(x = iwi, y = delta_y, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_stunted, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_stunted, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
    x = min(point.wise.absolute_diff_SS.Comb_stunted$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_stunted$RE.meta.upper) + 0.1, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_stunted$iwi), max(point.wise.absolute_diff_SS.Comb_stunted$iwi), by = 10))

print(combined_plot_stunted)


#ggsave(here::here("output", "combined-plot-stunted.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_stunted)


##############
##############
### Wasting


render(here::here("R", "gam-meta-analysis",
                  "pointwise-pooled-wasted-arm-maternal.Rmd"))


render(here::here("R", "linear-meta-analysis",
                  "linear-interaction-wasted.Rmd"))


point.wise.absolute_diff_SS.Comb_wasted <- point.wise.absolute_diff_SS.Comb_wasted %>% mutate(Model = "GAM")
plot_diff <- plot_diff %>% mutate(Model = "linear")
point.wise.absolute_diff <- point.wise.absolute_diff %>% mutate(Model = "linear")

combined_plot_wasted <- ggplot() +
  geom_line(data = plot_diff, aes(x = iwi, y = delta_y, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_wasted, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_wasted, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
    x = min(point.wise.absolute_diff_SS.Comb_wasted$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_wasted$RE.meta.upper) + 0.1, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_wasted$iwi), max(point.wise.absolute_diff_SS.Comb_wasted$iwi), by = 10))

print(combined_plot_wasted)


#ggsave(here::here("output", "combined-plot-wasted.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_wasted)


##############
##############
### Severe stunting


render(here::here("R", "gam-meta-analysis",
                  "pointwise-pooled-severe-stunted-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis",
                  "linear-interaction-severe-stunted.Rmd"))


point.wise.absolute_diff_SS.Comb_severe_stunted <- point.wise.absolute_diff_SS.Comb_severe_stunted %>% mutate(Model = "GAM")
plot_diff <- plot_diff %>% mutate(Model = "linear")
point.wise.absolute_diff <- point.wise.absolute_diff %>% mutate(Model = "linear")

combined_plot_severe_stunted <- ggplot() +
  geom_line(data = plot_diff, aes(x = iwi, y = delta_y, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_severe_stunted, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_severe_stunted, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
    x = min(point.wise.absolute_diff_SS.Comb_severe_stunted$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_severe_stunted$RE.meta.upper) + 0.1, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_severe_stunted$iwi), max(point.wise.absolute_diff_SS.Comb_severe_stunted$iwi), by = 10))

print(combined_plot_severe_stunted)


#ggsave(here::here("output", "combined-plot-severe-wasted.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_severe_stunted)


##############
##############
### Language score


render(here::here("R", "gam-meta-analysis",
                  "pointwise-pooled-language-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis",
                  "linear-interaction-language.Rmd"))


point.wise.absolute_diff_SS.Comb_language <- point.wise.absolute_diff_SS.Comb_language %>% mutate(Model = "GAM")
plot_diff <- plot_diff %>% mutate(Model = "linear")
point.wise.absolute_diff <- point.wise.absolute_diff %>% mutate(Model = "linear")


combined_plot_language <- ggplot() +
  geom_line(data = plot_diff, aes(x = iwi, y = delta_y, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_language, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_language, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
    x = min(point.wise.absolute_diff_SS.Comb_language$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_language$RE.meta.upper) + 0.1, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_language$iwi), max(point.wise.absolute_diff_SS.Comb_language$iwi), by = 10))

print(combined_plot_language)

#ggsave(here::here("output", "combined-plot-language.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_language)


##############
##############
### Motor score


render(here::here("R", "gam-meta-analysis",
                  "pointwise-pooled-gross-motor-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis",
                  "linear-interaction-gross.Rmd"))


point.wise.absolute_diff_SS.Comb_gross <- point.wise.absolute_diff_SS.Comb_gross %>% mutate(Model = "GAM")
plot_diff <- plot_diff %>% mutate(Model = "linear")
point.wise.absolute_diff <- point.wise.absolute_diff %>% mutate(Model = "linear")


combined_plot_gross <- ggplot() +
  geom_line(data = plot_diff, aes(x = iwi, y = delta_y, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_gross, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_gross, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
    x = min(point.wise.absolute_diff_SS.Comb_gross$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_gross$RE.meta.upper) + 0.1, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_gross$iwi), max(point.wise.absolute_diff_SS.Comb_gross$iwi), by = 10))

print(combined_plot_gross)

#ggsave(here::here("output", "combined-plot-gross.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_gross)


##############
##############
### Fine motor score


render(here::here("R", "gam-meta-analysis",
                  "pointwise-pooled-fine-motor-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis",
                  "linear-interaction-fine.Rmd"))

point.wise.absolute_diff_SS.Comb_fine <- point.wise.absolute_diff_SS.Comb_fine %>% mutate(Model = "GAM")
plot_diff <- plot_diff %>% mutate(Model = "linear")
point.wise.absolute_diff <- point.wise.absolute_diff %>% mutate(Model = "linear")


combined_plot_fine <- ggplot() +
  geom_line(data = plot_diff, aes(x = iwi, y = delta_y, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_fine, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_fine, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
    x = min(point.wise.absolute_diff_SS.Comb_fine$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_fine$RE.meta.upper) + 0.1, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_fine$iwi), max(point.wise.absolute_diff_SS.Comb_fine$iwi), by = 10))

print(combined_plot_fine)

#ggsave(here::here("output", "combined-plot-fine.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_fine)


##############
##############
### Executive function score


render(here::here("R", "gam-meta-analysis",
                  "pointwise-pooled-executive-function-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis",
                  "linear-interaction-exec.Rmd"))

point.wise.absolute_diff_SS.Comb_exec <- point.wise.absolute_diff_SS.Comb_exec %>% mutate(Model = "GAM")
plot_diff <- plot_diff %>% mutate(Model = "linear")
point.wise.absolute_diff <- point.wise.absolute_diff %>% mutate(Model = "linear")


combined_plot_exec <- ggplot() +
  geom_line(data = plot_diff, aes(x = iwi, y = delta_y, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_exec, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_exec, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
    x = min(point.wise.absolute_diff_SS.Comb_exec$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_exec$RE.meta.upper) + 0.1, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_exec$iwi), max(point.wise.absolute_diff_SS.Comb_exec$iwi), by = 10))

print(combined_plot_exec)

#ggsave(here::here("output", "combined-plot-exec.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_exec)


##############
##############
### Socioemotional score


render(here::here("R", "gam-meta-analysis",
                  "pointwise-pooled-socioemotional-score-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis",
                  "linear-interaction-socioemo.Rmd"))

point.wise.absolute_diff_SS.Comb_socioemo <- point.wise.absolute_diff_SS.Comb_socioemo %>% mutate(Model = "GAM")
plot_diff <- plot_diff %>% mutate(Model = "linear")
point.wise.absolute_diff <- point.wise.absolute_diff %>% mutate(Model = "linear")


combined_plot_socioemo <- ggplot() +
  geom_line(data = plot_diff, aes(x = iwi, y = delta_y, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_socioemo, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_socioemo, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
    x = min(point.wise.absolute_diff_SS.Comb_socioemo$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_socioemo$RE.meta.upper) + 0.04, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_socioemo$iwi), max(point.wise.absolute_diff_SS.Comb_socioemo$iwi), by = 10))

print(combined_plot_socioemo)

#ggsave(here::here("output", "combined-plot-socioemo.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_socioemo)


##############
##############
### Hemoglobin concentration


render(here::here("R", "gam-meta-analysis",
                  "pointwise-pooled-hemoglobin-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis",
                  "linear-interaction-hgb.Rmd"))

point.wise.absolute_diff_SS.Comb_hgb <- point.wise.absolute_diff_SS.Comb_hgb %>% mutate(Model = "GAM")
plot_diff <- plot_diff %>% mutate(Model = "linear")
point.wise.absolute_diff <- point.wise.absolute_diff %>% mutate(Model = "linear")


combined_plot_hgb <- ggplot() +
  geom_line(data = plot_diff, aes(x = iwi, y = delta_y, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_hgb, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_hgb, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
  ylim(c(0, 8)) +
  annotate(
    "text", 
    x = min(point.wise.absolute_diff_SS.Comb_hgb$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_hgb$RE.meta.upper) + 0.6, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_hgb$iwi), max(point.wise.absolute_diff_SS.Comb_hgb$iwi), by = 10))
  

print(combined_plot_hgb)

#ggsave(here::here("output", "combined-plot-hgb.png"), width = 8,
#       height = 6, dpi = 300, combined_plot_hgb)


##############
##############
### Anemia


render(here::here("R", "gam-meta-analysis",
                  "pointwise-pooled-anemia-arm-maternal.Rmd"))

render(here::here("R", "linear-meta-analysis",
                  "linear-interaction-anemia.Rmd"))

point.wise.absolute_diff_SS.Comb_anemia <- point.wise.absolute_diff_SS.Comb_anemia %>% mutate(Model = "GAM")
plot_diff <- plot_diff %>% mutate(Model = "linear")
point.wise.absolute_diff <- point.wise.absolute_diff %>% mutate(Model = "linear")
  

combined_plot_anemia <- ggplot() +
  geom_line(data = plot_diff, aes(x = iwi, y = delta_y, fill = Model, linetype = Model, color = Model), size = 0.5) + # linear pooled point estimates
  # geom_ribbon(data = point.wise.absolute_diff, aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # linear CIs
  geom_line(data = point.wise.absolute_diff_SS.Comb_anemia, aes(x = iwi, y = RE.meta, fill = Model, linetype = Model, color = Model), size = 0.5) + # GAM pooled point estimates
  geom_ribbon(data = filter(point.wise.absolute_diff_SS.Comb_anemia, Model == "GAM"), aes(x = iwi, ymin = RE.meta.lower, ymax = RE.meta.upper, fill = Model), alpha = 0.25) + # GAM CIs
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
  ylim(c(-0.2, 0.2)) +
  annotate(
    "text", 
    x = min(point.wise.absolute_diff_SS.Comb_anemia$iwi) * 0.8, 
    y = max(point.wise.absolute_diff_SS.Comb_anemia$RE.meta.upper) + 0.04, 
    label = paste("Interaction P-value =", round(as.numeric(unique(laz_eff_df_wide$pval.random)), 3)), 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black"
  ) +
  scale_color_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_fill_manual(values = c("GAM" = "brown", "linear" = "black")) +
  scale_x_continuous(breaks = seq(min(point.wise.absolute_diff_SS.Comb_anemia$iwi), max(point.wise.absolute_diff_SS.Comb_anemia$iwi), by = 10))

print(combined_plot_anemia)

#ggsave(here::here("output", "combined-plot-anemia.png"), width = 8,
#      height = 6, dpi = 300, combined_plot_anemia)

