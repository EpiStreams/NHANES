
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
    age_cat = cut(RIDAGEYR, c(0, 20, 40, 60, Inf), labels = FALSE, right = FALSE, ordered_result = TRUE),
      
    race_mid = case_when(
      RIDRETH3 %in% c(1,2) ~ 2, # Hispanic
      RIDRETH3 %in% c(3) ~ 1,   # NH White
      RIDRETH3 %in% c(4) ~ 3,   # NH Black
      RIDRETH3 %in% c(6) ~ 4,   # NH Asian
      RIDRETH3 %in% c(7) ~ 5),  # Other race
    
    race_cat = factor(x = race_mid, levels = 1:5, labels = c("Non-Hispanic White",
                      "Hispanic", "Non-Hispanic Black", "Non-Hispanic Asian", "Other race")),
    
    gender_cat = factor(x = RIAGENDR, levels = 1:2, labels = c("Male", "Female")),
    
    edu_cat_mid = case_when(
      DMDEDUC2 %in% c(1:3) ~ 1, #High school or less
      DMDEDUC2 %in% c(4) ~ 2, #Some college/Associates
      DMDEDUC2 %in% c(5) ~ 3, #College graduate
      TRUE ~ NA_real_),

    edu_cat = factor(x = edu_cat_mid, levels = 1:3, labels = c("High school or less", "Some college", 
                      "College graduate")),
    
    income_mid = cut(INDFMPIR, c(0, 1.30, 3.50, Inf), labels = FALSE, include.lowest = TRUE, 
                     right = TRUE, ordered_result = TRUE),
    
    income_cat = factor(x = income_mid, levels = 1:3, labels = c("<=130", ">130 to <=350", ">350")),
      
    WTMEC4YR = WTMEC2YR/2) %>% 
  
  select(-c(income_mid, edu_cat_mid, race_mid))



# Getting cohorts together

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









# Survey Objects ----------------------------------------------------------

#Education
object_full_edu_cohort <- subset(as_survey_design(full_edu_cohort, ids = SDMVPSU, strata = SDMVSTRA,
                                nest = TRUE, weights = WTMEC4YR), inc == 1)

object_htn_edu_cohort <- subset(as_survey_design(htn_edu_cohort, ids = SDMVPSU, strata = SDMVSTRA,
                                                 nest = TRUE, weights = WTMEC4YR), inc == 1)

object_med_edu_cohort <- subset(as_survey_design(med_edu_cohort, ids = SDMVPSU, strata = SDMVSTRA,
                                                 nest = TRUE, weights = WTMEC4YR), inc == 1)



#Income
object_full_inc_cohort <- subset(as_survey_design(full_inc_cohort, ids = SDMVPSU, strata = SDMVSTRA,
                                                  nest = TRUE, weights = WTMEC4YR), inc == 1)

object_htn_inc_cohort <- subset(as_survey_design(htn_inc_cohort, ids = SDMVPSU, strata = SDMVSTRA,
                                                  nest = TRUE, weights = WTMEC4YR), inc == 1) 

object_med_inc_cohort <- subset(as_survey_design(med_inc_cohort, ids = SDMVPSU, strata = SDMVSTRA,
                                                 nest = TRUE, weights = WTMEC4YR), inc == 1)





# Analysis (maybe) --------------------------------------------------------

popage <- c(77670, 72816, 45364)


full_edu_adjust_overall <- svystandardize(design = object_full_edu_cohort, by = ~age_cat, 
                                    over = ~ edu_cat, population = popage, excluding.missing = ~ htn) %>%
    group_by(edu_cat) %>%
    summarise(
    n = unweighted(n()),
    prop = survey_mean(htn == 1, na.rm = TRUE, vartype = "ci", level = 0.95)) %>%
    mutate(
    prop = prop*100,
    prop_low = prop_low*100,
    prop_upp = prop_upp*100)



