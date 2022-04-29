
# installing packages -----------------------------------------------------
install.packages("survey")
install.packages("srvyr")

# loading packages --------------------------------------------------------
library(tidyverse)
library(survey)
library(srvyr)


#creating stacked demographic data frame
demo_1516 <- read_rds("rds_data/demo_i") %>% 
  select(SEQN, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH3, DMDEDUC2, WTMEC2YR, SDMVPSU, SDMVSTRA, INDFMPIR)
demo_1718 <- read_rds("rds_data/demo_j") %>% 
  select(SEQN, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH3, DMDEDUC2, WTMEC2YR, SDMVPSU, SDMVSTRA, INDFMPIR)
demographics <- bind_rows(
  demo_1516,
  demo_1718
)


#next step is to merge each cohort with the stacked demographic data, which will create the analysis data frames