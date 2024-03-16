rm(list=ls(all=TRUE))

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

base_path <- "/Users/pearlante/Library/CloudStorage/Box-Box/sq-lns-equity-planetary-health/Data"

df_analysis <- read_dta(file = here::here(base_path, "0-untouched", "IPD for previous analysis",
                      "IPD_full_20240221.dta")) %>%
                       filter(!is.na(laz_bl)) %>% # Length/height-for-age Z-score at baseline
                       filter(!is.na(wlz_bl)) %>% # Weight-for-length/height Z-score at baseline
                       filter(!is.na(stunted_bl)) %>% # stunting at baseline
                       filter(!is.na(wasted_bl)) %>% # wasting at baseline
                       mutate(severe_stunted_bl = ifelse(laz_bl < -3, 1, 0), # severe stunting at baseline
                             severe_stunted_bl = factor(severe_stunted_bl, level=c(0,1), label=c("0","1"))) %>%
                       mutate(severe_wasted_bl = ifelse(wlz_bl < -3, 1, 0), # severe wasting at baseline
                              severe_wasted_bl = factor(severe_wasted_bl, level=c(0,1), label=c("0","1")))