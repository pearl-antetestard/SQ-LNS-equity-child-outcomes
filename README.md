# SQ-LNS-equity-child-outcomes

**Analysis code for assessing equity in the effects of small-quantity lipid-based nutrient supplements on child growth, development, and anemia**

## Overview

This repository contains R scripts, R Markdown documents, and supporting functions for the manuscript:

**Assessing equity in effects of small-quantity lipid-based nutrient supplements on child growth, development and anemia**

The code covers wealth-index processing, primary and supplementary analyses, pointwise meta-analysis, and generation of the manuscript figures and supplementary materials.

**Authors:** PA Ante-Testard, CD Arnold, KR Wessells, S Adu-Afarwuah, P Ashorn, E Becquey, KH Brown, P Christian, JM Colford, Jr, L LCH Fernald, E Galasso, SY Hess, JH Humphrey, L Huybregts, LL Iannotti, SP Luby, K Maleta, C Null, AJ Prendergast, AM Weber, H Ali, S Ali, U Ashorn, J Bendabenda, B Chasekwa, L Diop, SJL Dulience, K Jannat, C Kumwenda, A Lartey, A Le Port, J Leroy, C Mangani, S Matias, MK Mridha, R Ntozini, H Okronipa, JB Ouedraogo, J Phuka, M Rahman, L Ratsifandrihamanana, S Shaikh, AA Shamim, N Tavengwa, M Touré, P Wolff, T Benmarhnia, CP Stewart, KG Dewey, BF Arnold.

## Contact

For questions about the code or assistance contacting the study principal investigators, please contact:

**Pearl Ante-Testard**  
University of California, San Francisco  
[pearl.ante@ucsf.edu](mailto:pearl.ante@ucsf.edu)

## Zenodo and OSF

A mirror of this repository and the study pre-analysis plan are archived on Zenodo:

**[https://doi.org/10.5281/zenodo.24178656](https://doi.org/10.5281/zenodo.24178656)**

It is also archived on the Open Science Framework (OSF) as read-only:

**[https://osf.io/c7f5g/](https://osf.io/c7f5g/)**


Simulated datasets are available within the OSF project under **Script > Simulated data**.

## Data availability

### Original study data

The analyses combine data contributed by multiple principal investigators and institutions. Access to the original study data is available upon reasonable request to the individual principal investigators, subject to the applicable data-sharing requirements.

The corresponding author can facilitate contact with the relevant principal investigators. Reproducing the manuscript's numerical results requires access to the original study data.

### Simulated data

Simulated datasets in `.csv` and `.rds` formats are provided to demonstrate and test the analysis workflow. These datasets contain synthetic observations.

The supplied example includes length-for-age z score (LAZ), stored in the variable `laz_el`. To test scripts for other outcomes, generate compatible simulated variables with appropriate values, coding, and names. Examples of names expected by outcome-specific scripts include:

| Outcome | Variable name |
| --- | --- |
| Length-for-age z score (LAZ) | `laz_el` |
| Weight-for-length z score (WLZ) | `wlz_el` |
| Stunting | `stunted_el` |

For studies without a clustered design, set the cluster identifier to `NA`, following the format expected by the analysis scripts.

Additional simulated data can be generated using `simulate-data.R`.

## Software requirements

The analyses use R and the RStudio IDE. Rendering the `.Rmd` files also requires the R Markdown packages and Pandoc.

**Final analysis environment:** [Confirm the R and macOS versions used to generate the final manuscript results.]

The recorded session information supplied for this project is:

```text
R version 4.2.1 (2022-06-23)
Platform: aarch64-apple-darwin20 (64-bit)
Running under: macOS Sequoia 15.4
```

<!-- The original description also listed R 4.3.3 on macOS Ventura. Confirm which environment was used for the final analyses, then update this section and remove this comment. -->

Required R packages are identified in `0-config.R`. Record the package versions used for the final analyses with `sessionInfo()`.

## Installation

1. Install [R from CRAN](https://cran.r-project.org/), using the version documented for the final analysis environment.
2. Install the [RStudio IDE](https://posit.co/download/rstudio-desktop/).
3. Review `0-config.R` and install any required R packages that are not already available.
4. Run `0-config.R` to load the required packages and configure the analysis environment.

**Estimated installation time:** approximately 10 minutes on a typical desktop computer. The actual time depends on the network connection and whether packages need to be compiled.

## Getting started

1. Clone the complete repository:

   ```bash
   git clone https://github.com/pearl-antetestard/SQ-LNS-equity-child-outcomes.git
   cd SQ-LNS-equity-child-outcomes
   ```

2. Start a fresh R session with the repository root as the working directory.
3. Obtain either the simulated data from OSF or the original study data through the access process described above.
4. Check the input and output paths in `0-config.R` and the analysis scripts. Place the input files in the locations expected by the code.
5. Run the configuration script, then the data-processing and analysis files needed for the desired outputs.
6. Execute `.R` files in R or RStudio. Render `.Rmd` documents using RStudio's **Knit** button or `rmarkdown::render()`, ensuring that the required configuration and helper functions are loaded in the rendering environment.

The figure mappings below identify the relevant driver files and dependencies. Follow the dependencies defined within those files when running the analyses.

## Main manuscript figures

Four main driver files generate Figures 1–6.

| Output | Main driver file | Supporting files or folders |
| --- | --- | --- |
| Figure 1 | `02-Figure_1-pointwise-pooled-laz-arm-maternal.Rmd` | Dependencies specified within the document |
| Figure 2 | `03-Figure_2-stacked-plot-iwi-studies.Rmd` | Dependencies specified within the document |
| Figure 3 | `04-Figure_3-rii-sii-all-RUN-ALL.R` | `Figure_3-rii-sii-ineq/` |
| Figures 4, 5, and 6 | `05-Figures_4_5_6-gam-linear-pointwise-meta-RUN-ALL.Rmd` | `Figures_4_5_6_supplementary-gam-meta-analysis/`, `Figures_4_5_6-linear-meta-analysis/`, and `comb-linear-gam-meta-plot.R` |

## Supporting scripts and folders

| File or folder | Purpose |
| --- | --- |
| `0-config.R` | Defines package requirements and configures the analysis environment. |
| `01-iwi-processing/` | Contains scripts to calculate the International Wealth Index (IWI) and join the resulting index to each study's data. |
| `Figure_3-rii-sii-ineq/` | Contains supporting analyses called by the Figure 3 driver script. |
| `Figures_4_5_6_supplementary-gam-meta-analysis/` | Contains generalized additive model (GAM) scripts for the primary analysis, including maternal SQ-LNS where provided in the trial, and the supplementary analysis restricted to child SQ-LNS. |
| `Figures_4_5_6-linear-meta-analysis/` | Contains scripts to calculate interaction p-values for the primary analysis, including maternal SQ-LNS where provided in the trial. |
| `Figures_4_5_6-supplementary-linear-meta-analysis/` | Contains scripts to calculate interaction p-values for the supplementary analysis restricted to child SQ-LNS. |
| `comb-linear-gam-meta-plot.R` | Contains plotting code needed by the Figures 4–6 driver. |
| `func.R` | Contains functions used for the pointwise meta-analysis. |
| `pointwise-meta-analysis-all.Rmd` | Runs the pointwise meta-analysis scripts. |
| `supplementary-figures/` | Contains scripts for generating supplementary materials. |
| `simulate-data.R` | Generates synthetic data for demonstrating and testing the analysis code. |

## Reproducibility notes

- Use the corresponding code and data versions when reproducing the manuscript analyses.
- Begin with a fresh R session without restoring a previously saved workspace.
- Include the configuration file and all helper scripts called by the analysis drivers.
- Confirm that simulated outcomes have the types, coding, and variable names required by each analysis.
- Record R and package versions at the end of each analysis session using:

  ```r
  sessionInfo()
  ```


