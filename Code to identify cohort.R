
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
demo_1516 <- read_rds("rds_data/demo_i")
bpm_1516 <- read_rds("rds_data/bpx_i")
bpq_1516 <- read_rds("rds_data/bpq_i")

n1516_1 <- demo_1516 %>% 
  filter(WTMEC2YR > 0) %>% 
  filter(RIDAGEYR >= 18) %>% 
  filter(RIDEXPRG != 1 | is.na(RIDEXPRG)) %>% 
  left_join(bpm_1516, by = "SEQN") %>% 
  mutate(
    sbp_sum = rowSums(.[c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4")], na.rm = TRUE),
    dbp_sum = rowSums(.[c("BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")], na.rm = TRUE)) %>% 
  filter(sbp_sum > 0 & dbp_sum > 0) %>% 
  left_join(bpq_1516, by = "SEQN")
