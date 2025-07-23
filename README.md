# SQ-LNS-equity-child-outcomes

## Description
This repository includes R codes to support the paper:

_"Assessing equity in effects of small-quantity lipid-based nutrient supplements on child growth, development and anemia"_

PA Ante-Testard, CD Arnold, KR Wessells, S Adu-Afarwuah, P Ashorn, E Becquey, KH Brown, P Christian, JM Colford, Jr, L LCH Fernald, E Galasso, SY Hess, JH Humphrey, L Huybregts, LL Iannotti, SP Luby, K Maleta, C Null, AJ Prendergast, AM Weber, H Ali, S Ali, U Ashorn, J Bendabenda, B Chasekwa, L Diop, SJL Dulience, K Jannat, C Kumwenda, A Lartey, A Le Port, J Leroy, C Mangani, S Matias, MK Mridha, R Ntozini, H Okronipa, JB Ouedraogo, J Phuka, M Rahman, L Ratsifandrihamanana, M Ruel, S Shaikh, AA Shamim, N Tavengwa, M Touré, P Wolff, T Benmarhnia, CP Stewart, KG Dewey, BF Arnold

If there are any inquiries regarding the files in this repository, please feel free to contact Pearl Ante-Testard at UCSF (pearl.ante@ucsf.edu).

## Additional Resources

### Open Science Framework

This repo is mirrored to the Open Science Framework, where we also archived the pre-analysis plan the study:  https://osf.io/c7f5g/.

### Systems Requirement

All analyses were running using R software version 4.3.3 on Mac OSX Ventura using the RStudio IDE (https://www.rstudio.com). 

`> sessionInfo()`

`R version 4.2.1 (2022-06-23)`

`Platform: aarch64-apple-darwin20 (64-bit)`

`Running under: macOS Sequoia 15.4`

### Installation Guide

You can download and install R from CRAN: https://cran.r-project.org

You can download and install RStudio from their website: https://www.rstudio.com

All R packages required to run the analyses are sourced in the file `0-config.R`.

The installation time should be < 10 minutes total on a typical desktop computer.

### Instructions for Use

To run the analyses:

1. Clone this GitHub repository.
2. Start with the script `0-config.R` to install the required packages and set up the environment.
3. There is a simulated dataset (.csv and .rds) to test/run the codes. This is not a real data. To access the simulated dataset on OSF: click on Script > Simulated data. Just note that the simulated outcome here is laz (variable name: laz_el), so it won't run for other scripts adapted to other outcomes, unless the variable names are changed accordingly (e.g., wlz_el for wlz, stunted_el for stunting), and the clusters are set to NA for the studies that do not have clusters. You can also make your own simulated data using the script `simulate-data.R`.
4. The scripts to reproduce the Figures are numbered sequentially from 1 to 6.

Note: The data that support the findings of this analysis are a combination of data from multiple principal investigators and institutions. The data are available, upon reasonable request, to the requestor by contacting the individual principal investigators. The corresponding author can facilitate contact with the individual principal investigators upon request.

