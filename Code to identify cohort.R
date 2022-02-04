
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

cohort_1516 <- demo_1516 %>% 
  filter(WTMEC2YR > 0) %>% 
  filter(RIDAGEYR >= 18) %>% 
  filter(RIDEXPRG != 1 | is.na(RIDEXPRG)) %>% 
  left_join(bpm_1516, by = "SEQN") %>% 
  mutate(
    sbp_sum = rowSums(.[c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4")], na.rm = TRUE),
    sbp_count = rowSums(!is.na(.[c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4")])),
    dbp_sum = rowSums(.[c("BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")], na.rm = TRUE),
    dbp_count = rowSums(!is.na(.[c("BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")]))
    ) %>% 
  filter(sbp_sum != 0 & dbp_sum !=0) %>% 
  left_join(bpq_1516, by = "SEQN") %>% 
  filter(BPQ020 != 9) %>% 
  mutate(
    sbp_avg = sbp_sum/sbp_count,
    dbp_avg = dbp_sum/dbp_count,
    htn_bp = if_else(sbp_avg < 140 & dbp_avg < 90, true =  0, false = 1),
    htn_meds = case_when(
                          BPQ020 == 2 ~ 0,
                          BPQ040A == 2 | BPQ040A == 9 ~ 0,
                          BPQ050A == 2 ~ 0,
                          BPQ050A == 1 ~ 1,
                          TRUE ~ NA_real_),
    htn = if_else((htn_bp == 1 | htn_meds ==1), true = 1, false =  0))%>% 
  filter(htn == 1) %>% 
  filter(htn_meds == 1)




# 2017-2018
demo_1718 <- read_rds("rds_data/demo_j")
bpm_1718 <- read_rds("rds_data/bpx_j")
bpq_1718 <- read_rds("rds_data/bpq_j")

cohort_1718 <- demo_1718 %>% 
  filter(WTMEC2YR > 0) %>% 
  filter(RIDAGEYR >= 18) %>% 
  filter(RIDEXPRG != 1 | is.na(RIDEXPRG)) %>% 
  left_join(bpm_1718, by = "SEQN") %>% 
  mutate(
    sbp_sum = rowSums(.[c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4")], na.rm = TRUE),
    sbp_count = rowSums(!is.na(.[c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4")])),
    dbp_sum = rowSums(.[c("BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")], na.rm = TRUE),
    dbp_count = rowSums(!is.na(.[c("BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")]))
  )%>% 
  filter(sbp_sum != 0 & dbp_sum !=0) %>% 
  left_join(bpq_1718, by = "SEQN") %>% 
  filter(BPQ020 != 9) %>% 
  mutate(
    sbp_avg = sbp_sum/sbp_count,
    dbp_avg = dbp_sum/dbp_count,
    htn_bp = if_else(sbp_avg < 140 & dbp_avg < 90, true =  0, false = 1),
    htn_meds = case_when(
      BPQ020 == 2 ~ 0,
      BPQ040A == 2 | BPQ040A == 9 ~ 0,
      BPQ050A == 2 ~ 0,
      BPQ050A == 1 ~ 1,
      TRUE ~ NA_real_),
    htn = if_else((htn_bp == 1 | htn_meds ==1), true = 1, false =  0))%>% 
  filter(htn == 1) %>% 
  filter(htn_meds == 1)
  

  
