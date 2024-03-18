### This script is to clean, create new variables and
### save formatted data for growth outcomes.
### Author: Pearl Ante-Testard
### Date: 2024-03-17
### E-mail: pearl.ante@ucsf.edu

# Clean workspace
rm(list = ls(all = TRUE))

# Load libraries
library(here)
library(haven)
library(dplyr)

# Load data and create new variables
df_analysis <- read_dta(file = here::here("data", "0-untouched",
                                          "IPD_full_20200905.dta")) %>%
# baseline variables # nolint
  mutate(severe_stunted_bl = ifelse(laz_bl < -3, 1, 0)) %>% # severe stunting at baseline # nolint
  mutate(severe_stunted_bl = factor(severe_stunted_bl, levels=c(0,1), labels=c("0","1"))) %>% # nolint
  mutate(severe_wasted_bl = ifelse(wlz_bl < -3, 1, 0)) %>% # severe wasting at baseline # nolint
  mutate(severe_wasted_bl = factor(severe_wasted_bl, levels=c(0,1), labels=c("0","1"))) %>% # nolint
  # endline variables
  mutate(severe_stunted_el = ifelse(laz_el < -3, 1, 0)) %>% # severe stunting at endline # nolint
  mutate(severe_stunted_el = factor(severe_stunted_el, levels=c(0,1), labels=c("0","1"))) %>% # nolint
  mutate(severe_wasted_el = ifelse(wlz_el < -3, 1, 0)) %>% # severe wasting at endline # nolint
  mutate(severe_wasted_el = factor(severe_wasted_el, levels=c(0,1), labels=c("0","1"))) %>% # nolint # nolint
  mutate(study = sub("s[0-9]*_", "", study))

# Create a new variable for wealth rank based on the baseline sesindex variable
# for each study
df_analysis <- df_analysis %>%
  group_by(study) %>%
  mutate(wealth_rank = rank(sesindex) / n()) %>%
  ungroup()

# Save the formatted data
saveRDS(df_analysis, here::here("data", "1-final",
                                "IPD_full_20200905_growth_formatted.RDS"))
write.csv2(df_analysis, here::here("data", "1-final",
                                   "IPD_full_20200905_growth_formatted.csv"))