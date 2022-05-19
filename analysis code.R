
# installing packages -----------------------------------------------------
install.packages("survey")
install.packages("srvyr")

# loading packages --------------------------------------------------------
library(tidyverse)
library(survey)
library(srvyr)



# Creating data frames for each cohort ------------------------------------

# start with creating stacked demographic data frame (contains all but the htn variables)
demo_1516 <- read_rds("rds_data/demo_i") %>% 
  select(SEQN, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH3, DMDEDUC2, WTMEC2YR, SDMVPSU, SDMVSTRA, INDFMPIR)
demo_1718 <- read_rds("rds_data/demo_j") %>% 
  select(SEQN, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH3, DMDEDUC2, WTMEC2YR, SDMVPSU, SDMVSTRA, INDFMPIR)
demographics <- bind_rows(
  demo_1516,
  demo_1718) %>% 
  
  mutate(
    age_cat = case_when(
      RIDAGEYR < 20 ~ "<20",
      RIDAGEYR >= 20 & RIDAGEYR < 40 ~ "20-39",
      RIDAGEYR >= 40 & RIDAGEYR < 60 ~ "40-59",
      RIDAGEYR >= 60 ~ ">=60")) %>% 
      
    mutate(
    race_cat = case_when(
      RIDRETH3 %in% c(1,2) ~ 2,
      RIDRETH3 %in% c(3) ~ 1,
      RIDRETH3 %in% c(4) ~ 3,
      RIDRETH3 %in% c(6) ~ 4,
      RIDRETH3 %in% c(7) ~ 5))

# data cleaning



### Education cohorts

# full education cohort
full_edu_cohort <- read_rds("rds_data/full_edu_cohort") %>% 
  full_join(
  .,
  demographics,
  by = "SEQN")

# education cohort restricted to those with hypertension
htn_edu_cohort <- read_rds("rds_data/htn_edu_cohort") %>% 
  full_join(
    .,
    demographics,
    by = "SEQN")

# education cohort restricted to those with hypertension taking antihypertensive medications
med_edu_cohort <- read_rds("rds_data/med_edu_cohort") %>% 
  full_join(
    .,
    demographics,
    by = "SEQN")






### Income cohorts

# full income cohort
full_inc_cohort <- read_rds("rds_data/full_inc_cohort") %>% 
  full_join(
    .,
    demographics,
    by = "SEQN")

# income cohort restricted to those with hypertension
htn_inc_cohort <- read_rds("rds_data/htn_inc_cohort") %>% 
  full_join(
    .,
    demographics,
    by = "SEQN")

# income cohort restricted to those with hypertension taking antihypertensive medications
med_inc_cohort <- read_rds("rds_data/med_inc_cohort") %>% 
  full_join(
    .,
    demographics,
    by = "SEQN")





# Data prep ----------------------------------------------------------


