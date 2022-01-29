
# Identifying cohort for analysis -----------------------------------------
library(tidyverse)


# Exclusions (from Muntner):
    ## Completed NHANES interview and Exam
    ## Ages 18 years and older
    ## Not pregnant
    ## Valid BPs
    ## Information on antihypertensive medication use
    ## Taking antihypertnesive medications


# Exclusions (mine):
    ## Same as Muntner +
    ## Information on income/information on education




# Replicating Muntner's Cohorts -------------------------------------------

# 2015-2016
n1516_1 <- read_rds("rds_data/demo_i")
nb1516 <- read_rds("rds_data/bpx_i")

n1516_2 <- n1516_1 %>% 
  filter(WTMEC2YR > 0) %>% 
  filter(RIDAGEYR >= 18) %>% 
  filter(RIDEXPRG != 1 | is.na(RIDEXPRG)) %>% 
  left_join(nb1516, by = "SEQN")
