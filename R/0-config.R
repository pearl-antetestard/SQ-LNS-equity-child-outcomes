
#----------------------------
# IPD analysis
#
# Configuration file
#
# Load packages and functions
#----------------------------


#----------------------------
# load worker packages
#----------------------------
library(here)
library(dplyr)
library(table1)
library(haven)
library(tidyverse)
library(readxl)
library(skimr)
library(rmarkdown)
library(stringr)

#----------------------------
# data visualization packages
#----------------------------
library(dplyr)
library(broom)
library(purrr)
library(ggsci)
library(ggrepel)
library(gridExtra)
library(cowplot)
library(RColorBrewer)
library(patchwork)

#----------------------------
# GAM, test, and meta-analysis packages
#----------------------------
library(mgcv)
library(meta)
library(tidymv)
library(lmtest)
library(msm)
library(sandwich)
library(estimatr)
library(geepack)

#----------------------------
# Meta-analysis functions
#----------------------------
# This is derived from the article by Belias et al. (2022): https://onlinelibrary.wiley.com/doi/10.1002/jrsm.1546
source(here::here("R", "func.R"))
