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
    id = 1:1000,
    year = runif(n = 1000, min = 1972, max = 2023) |> floor(),
    sex = runif(n = 1000, min = 1, max = 3) |> floor(),
    age = runif(n = 1000, min = 1, max = 100) |> floor(),
    mental_health = runif(n = 1000, min = 0, max = 32) |> floor(),
    physical_health = runif(n = 1000, min = 0, max = 32) |> floor(),
    depress = runif(n = 1000, min = 1, max = 3) |> floor(),
    health = runif(n = 1000, min = 1, max = 5) |> floor()
  )

# expected that id is only from 1 to 1000
# expected that year is only from 1972 to 2022
# expected that sex is only between 1 to 2
# expected that age is only between 1 to 99
# expected that mental_health is only between 0 to 31
# expected that  physical_health is only between 0 to 31
# expected that depress is only either 1 or 2
# expected that health is only between 1 to 4


#### Add tests for simulated data ####

# expected that id is only from 1 to 1000
unique(simulated_data$id) == c(1:1000)

# expected that year is only from 1972 to 2022
max(simulated_data$year) <= 2022
min(simulated_data$year) >= 1972

# expected that sex is only between 1 to 2
max(simulated_data$sex) == 2
min(simulated_data$sex) == 1
any(unique(simulated_data$sex) == c(1:2))

# expected that age is only between 1 to 99
min(simulated_data$age) >= 1
max(simulated_data$age) <= 99

# expected that mental_health is only between 0 to 31
min(simulated_data$mental_health) >= 0
max(simulated_data$mental_health) <= 31

# expected that  physical_health is only between 0 to 31
min(simulated_data$physical_health) >= 0
max(simulated_data$physical_health) <= 31

# expected that depress is only either 1 or 2
max(simulated_data$depress) == 2
min(simulated_data$depress) == 1
any(unique(simulated_data$depress) == c(1:2))

# expected that health is only between 1 to 4
max(simulated_data$health) <= 4
min(simulated_data$health) >= 1
any(unique(simulated_data$health) == c(1:4))
