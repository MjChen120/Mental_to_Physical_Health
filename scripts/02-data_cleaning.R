#### Preamble ####
# Purpose: Cleans the raw data recorded by U.S.General Social Survey (GSS)
# Author: Mingjia Chen
# Date: 28 March 2024
# Contact: mingjia.chen@mail.utoronto.ca
# License: MIT
# Pre-requisites: Have to manually download the data from the raw data folder.
# Any other information needed? -

#### Workspace setup ####
library(tidyverse)
library(arrow)
library(dplyr)

#### Load data ####
# This part of the code is from GSS Open Explore

# When downloading the dataset from the website, the R script can be chosen to
# be downloaded as well.

#After running this part of the code, the data will be available for modifying
# and analyzing.

library(foreign)
read.dct <- function(dct, labels.included = "yes") {
  temp <- readLines(dct)
  temp <- temp[grepl("_column", temp)]
  switch(labels.included,
         yes = {
           pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+)[a-z]\\s+(.*)"
           classes <- c("numeric", "character", "character", "numeric", "character")
           N <- 5
           NAMES <- c("StartPos", "Str", "ColName", "ColWidth", "ColLabel")
         },
         no = {
           pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+).*"
           classes <- c("numeric", "character", "character", "numeric")
           N <- 4
           NAMES <- c("StartPos", "Str", "ColName", "ColWidth")
         })
  temp_metadata <- setNames(lapply(1:N, function(x) {
    out <- gsub(pattern, paste("\\", x, sep = ""), temp)
    out <- gsub("^\\s+|\\s+$", "", out)
    out <- gsub('\"', "", out, fixed = TRUE)
    class(out) <- classes[x] ; out }), NAMES)
  temp_metadata[["ColName"]] <- make.names(gsub("\\s", "", temp_metadata[["ColName"]]))
  temp_metadata
}

read.dat <- function(dat, metadata_var, labels.included = "yes") {
  read.table(dat, col.names = metadata_var[["ColName"]])
}


GSS_metadata <- read.dct(here::here("data/raw_data/GSS.dct"))
GSS_ascii <- read.dat(here::here("data/raw_data/GSS.dat"), GSS_metadata)
attr(GSS_ascii, "col.label") <- GSS_metadata[["ColLabel"]]
GSS <- GSS_ascii


#### Clean data ####
# Actual Data cleaning

# Select desired variables and rename them for easier access/analyze 
GSS <- select(GSS,c("ID_","YEAR","SEX","AGE","HEALTH","PHYSHLTH","MNTLHLTH","DEPRESS"))
colnames(GSS) <- c("id","year","sex","age","health","phys_days","ment_days","depress")

# Since we have non-responses due to the fact that many variables were only collected in
# certain years, four separate datasets will be used for the paper.

# 1. Dataset for demographic variables
## Gender, Age, Numbers of days of Physical and Mental un-wellness. 
demo_data <- filter(GSS, ment_days >= 0) %>% filter(phys_days >= 0) %>% 
  filter(age >= 1) %>% filter(sex >= 1)
demo_data <- select(demo_data,c("id","sex","age","phys_days","ment_days"))

# change the sex variables from numbers to strings female and male
demo_data$sex <- as.character(demo_data$sex)
demo_data <- mutate(demo_data,
               sex = case_when(
                 sex == "1" ~ "Male",
                 sex == "2" ~ "Female",
                 TRUE ~ sex
               )
            )

#categorize ages into five cohorts 
#code referenced from https://pubs.wsb.wisc.edu/academics/analytics-using-r-2019/convert-numerical-data-to-categorical.html
demo_data <- within(demo_data, {   
  age_cohort <- NA # need to initialize variable
  age_cohort[age < 20] <- "0-19"
  age_cohort[age >= 20 & age < 40] <- "20-39"
  age_cohort[age >= 40 & age < 60] <- "40-59"
  age_cohort[age >= 60 & age < 80] <- "60-79"
  age_cohort[age >= 80] <- "80+"
} )
# 2. Dataset for the relationship between Numbers of days of Physical and Mental un-wellness
days_data <- filter(GSS, ment_days >= 0) %>% filter(phys_days >= 0)
days_data <- select(days_data,c("id","phys_days","ment_days"))

# 3. Dataset for the relationship between whether diagnozed as having depression 
# and Number of days of Physical un-wellness
depress_data <- filter(GSS, phys_days >= 0) %>% filter(depress >= 0)
depress_data <- select(depress_data,c("id","phys_days","depress"))

# 4. Dataset for the relationship between Numbers of days of Mental un-wellness
# and one's health status in general
mentalVsHealth_data <- filter(GSS, ment_days >= 0) %>% filter(health >= 0)
mentalVsHealth_data  <- select(mentalVsHealth_data,c("id","ment_days","health"))

#### Save data ####
write_parquet(GSS, here::here("data/analysis_data/cleaned_GSS.parquet"))
write_parquet(demo_data, here::here("data/analysis_data/demo_data.parquet"))
write_parquet(days_data, here::here("data/analysis_data/days_data.parquet"))
write_parquet(depress_data, here::here("data/analysis_data/depress_data.parquet"))
write_parquet(mentalVsHealth_data, here::here("data/analysis_data/mentalVsHealth_data.parquet"))
