
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
    ## Age >= 20 years +
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
  mutate_at(vars(starts_with("BPXDI")), list(~na_if(., 0))) %>%
  mutate(
    sbp_count = rowSums(!is.na(select(., starts_with("BPXSY")))),
    dbp_count = rowSums(!is.na(select(., starts_with("BPXDI"))))
    ) %>%
  filter(sbp_count > 0 & dbp_count > 0) %>% 
  mutate(
    sbp_avg = rowMeans(select(., starts_with("BPXSY")), na.rm = TRUE),
    dbp_avg = rowMeans(select(., starts_with("BPXDI")), na.rm = TRUE)) %>% 
  left_join(bpq_1516, by = "SEQN") %>% 
  filter(BPQ020 != 9) %>% 
  mutate(
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
  mutate_at(vars(starts_with("BPXDI")), list(~na_if(., 0))) %>%
  mutate(
    sbp_count = rowSums(!is.na(select(., starts_with("BPXSY")))),
    dbp_count = rowSums(!is.na(select(., starts_with("BPXDI"))))
  ) %>%
  filter(sbp_count > 0 & dbp_count > 0) %>% 
  mutate(
    sbp_avg = rowMeans(select(., starts_with("BPXSY")), na.rm = TRUE),
    dbp_avg = rowMeans(select(., starts_with("BPXDI")), na.rm = TRUE)) %>% 
  left_join(bpq_1718, by = "SEQN") %>% 
  filter(BPQ020 != 9) %>% 
  mutate(
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





# creating my cohorts ------------------------------------------------------

# 2015-2016 - differences include the age range (>=20) & the separate cohorts for education and income. Also restricting only to SQN number and 
# indicator of inclusion in the analysis.


demo_1516 <- read_rds("rds_data/demo_i")
bpm_1516 <- read_rds("rds_data/bpx_i")
bpq_1516 <- read_rds("rds_data/bpq_i")

cohort_1516 <- demo_1516 %>% 
  filter(WTMEC2YR > 0) %>% 
  filter(RIDAGEYR >= 20) %>% 
  filter(RIDEXPRG != 1 | is.na(RIDEXPRG)) %>% 
  left_join(bpm_1516, by = "SEQN") %>% 
  mutate_at(vars(starts_with("BPXDI")), list(~na_if(., 0))) %>%
  mutate(
    sbp_count = rowSums(!is.na(select(., starts_with("BPXSY")))),
    dbp_count = rowSums(!is.na(select(., starts_with("BPXDI"))))
  ) %>%
  filter(sbp_count > 0 & dbp_count > 0) %>% 
  mutate(
    sbp_avg = rowMeans(select(., starts_with("BPXSY")), na.rm = TRUE),
    dbp_avg = rowMeans(select(., starts_with("BPXDI")), na.rm = TRUE)) %>% 
  left_join(bpq_1516, by = "SEQN") %>% 
  filter(BPQ020 != 9) %>% 
  mutate(
    htn_bp = if_else(sbp_avg < 140 & dbp_avg < 90, true =  0, false = 1),
    htn_meds = case_when(
      BPQ020 == 2 ~ 0,
      BPQ040A == 2 | BPQ040A == 9 ~ 0,
      BPQ050A == 2 ~ 0,
      BPQ050A == 1 ~ 1,
      TRUE ~ NA_real_),
    htn = if_else((htn_bp == 1 | htn_meds ==1), true = 1, false =  0))


#education cohort (full)
ed_1516_all <- cohort_1516 %>% 
  filter(!(DMDEDUC2 %in% c(7,9))) %>% 
  mutate(
    inc = 1
  ) %>% 
  select(SEQN, htn, htn_bp, htn_meds, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH3, DMDEDUC2, WTMEC2YR, SDMVPSU, SDMVSTRA, INDFMPIR, inc)

#education cohort (htn)
ed_1516_htn <- cohort_1516 %>% 
  filter(!(DMDEDUC2 %in% c(7,9))) %>% 
  filter(htn == 1)%>% 
  mutate(
    inc = 1
  ) %>% 
  select(SEQN, htn, htn_bp, htn_meds, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH3, DMDEDUC2, WTMEC2YR, SDMVPSU, SDMVSTRA, INDFMPIR, inc)

#education cohort (meds)
ed_1516_med <- cohort_1516 %>% 
  filter(!(DMDEDUC2 %in% c(7,9))) %>% 
  filter(htn == 1) %>% 
  filter(htn_meds == 1)%>% 
  mutate(
    inc = 1
  ) %>% 
  select(SEQN, htn, htn_bp, htn_meds, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH3, DMDEDUC2, WTMEC2YR, SDMVPSU, SDMVSTRA, INDFMPIR, inc)




#income cohort (full)
inc_1516_all <- cohort_1516 %>% 
  filter(!is.na(INDFMPIR))%>% 
  mutate(
    inc = 1
  ) %>% 
  select(SEQN, htn, htn_bp, htn_meds, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH3, DMDEDUC2, WTMEC2YR, SDMVPSU, SDMVSTRA, INDFMPIR, inc)

#income cohort (htn)
inc_1516_htn <- cohort_1516 %>% 
  filter(!is.na(INDFMPIR)) %>% 
  filter(htn == 1)%>% 
  mutate(
    inc = 1
  ) %>% 
  select(SEQN, htn, htn_bp, htn_meds, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH3, DMDEDUC2, WTMEC2YR, SDMVPSU, SDMVSTRA, INDFMPIR, inc)

#income cohort (meds)
inc_1516_med <- cohort_1516 %>% 
  filter(!is.na(INDFMPIR)) %>% 
  filter(htn == 1) %>% 
  filter(htn_meds == 1)%>% 
  mutate(
    inc = 1
  ) %>% 
  select(SEQN, htn, htn_bp, htn_meds, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH3, DMDEDUC2, WTMEC2YR, SDMVPSU, SDMVSTRA, INDFMPIR, inc)









# 2017-2018 - differences include the age range (>=20) & the separate cohorts for education and income. Also restricting only to SQN number and 
# indicator of inclusion in the analysis.
demo_1718 <- read_rds("rds_data/demo_j")
bpm_1718 <- read_rds("rds_data/bpx_j")
bpq_1718 <- read_rds("rds_data/bpq_j")

cohort_1718 <- demo_1718 %>% 
  filter(WTMEC2YR > 0) %>% 
  filter(RIDAGEYR >= 20) %>% 
  filter(RIDEXPRG != 1 | is.na(RIDEXPRG)) %>% 
  left_join(bpm_1718, by = "SEQN") %>% 
  mutate_at(vars(starts_with("BPXDI")), list(~na_if(., 0))) %>%
  mutate(
    sbp_count = rowSums(!is.na(select(., starts_with("BPXSY")))),
    dbp_count = rowSums(!is.na(select(., starts_with("BPXDI"))))
  ) %>%
  filter(sbp_count > 0 & dbp_count > 0) %>% 
  mutate(
    sbp_avg = rowMeans(select(., starts_with("BPXSY")), na.rm = TRUE),
    dbp_avg = rowMeans(select(., starts_with("BPXDI")), na.rm = TRUE)) %>% 
  left_join(bpq_1718, by = "SEQN") %>% 
  filter(BPQ020 != 9) %>% 
  mutate(
    htn_bp = if_else(sbp_avg < 140 & dbp_avg < 90, true =  0, false = 1),
    htn_meds = case_when(
      BPQ020 == 2 ~ 0,
      BPQ040A == 2 | BPQ040A == 9 ~ 0,
      BPQ050A == 2 ~ 0,
      BPQ050A == 1 ~ 1,
      TRUE ~ NA_real_),
    htn = if_else((htn_bp == 1 | htn_meds ==1), true = 1, false =  0))


#education cohort (all)
ed_1718_all <- cohort_1718 %>%
  filter(!(DMDEDUC2 %in% c(7,9)))%>% 
  mutate(
    inc = 1
  ) %>% 
  select(SEQN, htn, htn_bp, htn_meds, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH3, DMDEDUC2, WTMEC2YR, SDMVPSU, SDMVSTRA, INDFMPIR, inc)
  

#education cohort (htn) 
ed_1718_htn <- cohort_1718 %>%
  filter(!(DMDEDUC2 %in% c(7,9))) %>% 
  filter(htn == 1)%>% 
  mutate(
    inc = 1
  ) %>% 
  select(SEQN, htn, htn_bp, htn_meds, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH3, DMDEDUC2, WTMEC2YR, SDMVPSU, SDMVSTRA, INDFMPIR, inc) 
  

#education cohort (meds)
ed_1718_med <- cohort_1718 %>%
  filter(!(DMDEDUC2 %in% c(7,9))) %>% 
  filter(htn == 1) %>% 
  filter(htn_meds == 1)%>% 
  mutate(
    inc = 1
  ) %>% 
  select(SEQN, htn, htn_bp, htn_meds, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH3, DMDEDUC2, WTMEC2YR, SDMVPSU, SDMVSTRA, INDFMPIR, inc)




#income cohort (all)
inc_1718_all <- cohort_1718 %>% 
  filter(!is.na(INDFMPIR))%>% 
  mutate(
    inc = 1
  ) %>% 
  select(SEQN, htn, htn_bp, htn_meds, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH3, DMDEDUC2, WTMEC2YR, SDMVPSU, SDMVSTRA, INDFMPIR, inc)


#income cohort (htn)
inc_1718_htn <- cohort_1718 %>% 
  filter(!is.na(INDFMPIR)) %>% 
  filter(htn == 1)%>% 
  mutate(
    inc = 1
  ) %>% 
  select(SEQN, htn, htn_bp, htn_meds, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH3, DMDEDUC2, WTMEC2YR, SDMVPSU, SDMVSTRA, INDFMPIR, inc)


#income cohort (med)
inc_1718_med <- cohort_1718 %>% 
  filter(!is.na(INDFMPIR)) %>% 
  filter(htn == 1) %>% 
  filter(htn_meds == 1)%>% 
  mutate(
    inc = 1
  ) %>% 
  select(SEQN, htn, htn_bp, htn_meds, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH3, DMDEDUC2, WTMEC2YR, SDMVPSU, SDMVSTRA, INDFMPIR, inc)









# binding rows to create cross year cohorts -------------------------------

  

full_edu_cohort <- bind_rows(ed_1516_all, ed_1718_all) %>% 
  write_rds("rds_data/full_edu_cohort")

htn_edu_cohort <- bind_rows(ed_1516_htn, ed_1718_htn)%>% 
  write_rds("rds_data/htn_edu_cohort")

med_edu_cohort <- bind_rows(ed_1516_med, ed_1718_med) %>% 
  write_rds("rds_data/med_edu_cohort")




full_inc_cohort <- bind_rows(inc_1516_all, inc_1718_all) %>% 
  write_rds("rds_data/full_inc_cohort")

htn_inc_cohort <- bind_rows(inc_1516_htn, inc_1718_htn) %>% 
  write_rds("rds_data/htn_inc_cohort")

med_inc_cohort <- bind_rows(inc_1516_med, inc_1718_med) %>% 
  write_rds("rds_data/med_inc_cohort")
