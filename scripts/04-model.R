#### Preamble ####
# Purpose: Models created and prepared for the analysis in paper.
# Author: Mingjia Chen
# Date: 31 March 2024
# Contact: mingjia.chen@mail.utoronto.ca
# License: MIT
# Pre-requisites: GSS.dct, GGS.dat, data_cleaning.R


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(arrow)

#### Read data ####
path <- "data/analysis_data/analysis_data.parquet"
analysis_data <- read_parquet(here::here(path))

### Model data ####
first_model <-
  stan_glm(
    formula = phys_days ~ ment_days + depress,
    data = analysis_data,
    family = gaussian(),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_aux = exponential(rate = 1, autoscale = TRUE),
    seed = 853
  )


#### Save model ####
saveRDS(
  first_model,
  file = "models/first_model.rds"
)
