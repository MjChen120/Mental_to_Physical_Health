#### Preamble ####
# Purpose: Simulates the raw dataset for code testing
# Author: Mingjia Chen
# Date: 28 March 2024
# Contact: mingjia.chen@mail.utoronto.ca
# License: MIT
# Pre-requisites: R


#### Workspace setup ####
library(tidyverse)

#### Simulate data ####
simulated_data <-
  tibble(
    mental_health =runif(n = 1000,min = 0,max = 7) |> floor(),
    physical_health = runif(n = 1000,min = 0,max = 7) |> floor(),
)

# expected that mental_health is only between 0 to 6
# expected that  physical_health is only between 0 to 6

#### Add tests for simulated data ####

# expected that mental_health is only between 0 to 6
any(unique(simulated_data$mental_health)) == c(0:6)
min(simulated_data$mental_health) == 0
max(simulated_data$mental_health) == 6
# expected that  physical_health is only between 0 to 6
any(unique(simulated_data$physical_health)) == c(0:6)
min(simulated_data$physical_health) == 0
max(simulated_data$physical_health) == 6
