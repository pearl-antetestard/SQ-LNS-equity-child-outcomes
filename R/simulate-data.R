## This script is to simulate data to run/test the codes.
## The simulated outcome here is laz (variable name: laz_el), so it won't run for other scripts adapted to other outcomes, unless
## the variable names are changed accordingly (e.g., wlz_el for wlz, stunted_el for stunting), and the clusters are set to NA 
## for the studies that do not have clusters.
## email: pearl.ante@ucsf.edu

# Load necessary library
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Number of samples
n_samples <- 1000

# Generate unique pids
pid <- 1:n_samples

# Define study labels
study_labels <- c("DOSE", "DYADG", "DYADM", "GHANA", "HAITI", "JiVitA", "MAHAY", "PROMISBF", "PROMISBF_CS", "PROMISM", "PROMISM_CS", "RDNS", "SHINE_HIV-", "WASHB", "WASHK", "ZINC")

# Generate study identifiers (randomly assigned 1 to 16 and then mapped to labels)
study <- sample(1:16, n_samples, replace = TRUE)
study <- factor(study, levels = 1:16, labels = study_labels)

# Generate cluster ids
# Studies "DOSE", "GHANA", "HAITI", "DYADG", "DYADM" will have NA cluster values
# Other studies will have cluster ids
cluster <- ifelse(study %in% c("DOSE", "GHANA", "HAITI", "DYADG", "DYADM"), NA, sample(1:100, n_samples, replace = TRUE))

# Generate binary arms_maternal (0 or 1) and rename them
arms_maternal <- sample(0:1, n_samples, replace = TRUE)
arms_maternal <- ifelse(arms_maternal == 0, "Control", "SQ-LNS")

# Generate iwi values from 0 to 100
iwi <- runif(n_samples, min = 0, max = 100)

# Generate laz_el values from -6 to 6
laz_el <- runif(n_samples, min = -6, max = 6)

# Dummy column with value 1 for all
dummy <- rep(1, n_samples)

# Create data frame
df_analysis_iwi <- data.frame(
  pid = pid,
  study = study,
  cluster = cluster,
  arms_maternal = arms_maternal,
  iwi = iwi,
  laz_el = laz_el,
  dummy = dummy
)
# Convert variables to factors
df_analysis_iwi <- df_analysis_iwi %>%
  mutate(
    arms_maternal = factor(arms_maternal, levels = c("Control", "SQ-LNS")),
    study = factor(study, levels = study_labels),
    cluster = factor(cluster)
  )

# Display the first few rows of the data frame
head(df_analysis_laz)

# Save the simulated data frame to a CSV file
write.csv(df_analysis_iwi, here::here("data",
                                      "2-public",
                                      "df_analysis_iwi.csv"), row.names = FALSE)
# Save the simulated data frame to an RDS file
saveRDS(df_analysis_iwi, here::here("data",
                                    "2-public",
                                    "df_analysis_iwi.rds"))



