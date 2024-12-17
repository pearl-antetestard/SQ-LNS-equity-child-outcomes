
rm(list=ls(all=TRUE))


df_analysis_long <- read_dta(file = here::here("data", "0-untouched", "long",
                                               "IPD_ucsf_20240212.dta"))

summary_by_month_study <- df_analysis_long %>%
  group_by(study, measmonth) %>%
  summarize(
    mean_wlz = mean(wlz, na.rm = TRUE),
    sd_wlz = sd(wlz, na.rm = TRUE),
    n = n()
  )

print(summary_by_month_study)

# Create a function to generate time series plots for a specific study

create_time_series_plot <- function(study_name, data) {
  data_filtered <- data %>% 
    filter(study == study_name & is.finite(measmonth)) %>%
    mutate(measmonth = round(measmonth, 2))
  
  ggplot(data_filtered, aes(x = measmonth, y = wlz)) +
    stat_summary(fun = mean, geom = "line", size = 1) +
    stat_summary(fun = mean, geom = "point", size = 2, color = "black") +
    xlab("Month") +
    ylab("WLZ") +
    ggtitle(paste("Seasonality of WLZ by Month (", study_name, ")", sep = "")) +
    theme_minimal() +
    theme(
      axis.title.y = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12)
    ) +
    scale_x_continuous(breaks = seq(min(data_filtered$measmonth), max(data_filtered$measmonth), by = 1))  # Smaller breaks
}


# Get the list of unique studies
study_list <- unique(df_analysis_long$study)

# Create a list to store the plots
plots <- list()

# Loop through each study and create the plot
for (study_name in study_list) {
  plots[[study_name]] <- create_time_series_plot(study_name, df_analysis_long)
}

# Alternatively, you can use purrr::map to create the plots
plots <- map(study_list, ~ create_time_series_plot(.x, df_analysis_long))

# Print the plots 
print(plots) # all studies
print(plots[[1]])
print(plots[[2]])
print(plots[[3]])
print(plots[[4]])
print(plots[[5]])
print(plots[[6]])
print(plots[[7]])
print(plots[[8]])
print(plots[[9]])
print(plots[[10]])
print(plots[[11]])
print(plots[[12]])
print(plots[[13]])
print(plots[[14]])
print(plots[[15]])

