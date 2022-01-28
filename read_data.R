
# Reading in NHANES data --------------------------------------------------




## SASxport package and tidyverse (just in case it's needed)
library(tidyverse)
library(SASxport)




## Demographic Data -

  ## 2017-2018
  demo_j <- read.xport("raw_data/DEMO_J.XPT")
  write_rds(demo_j, "rds_data/demo_j")

  ## 2015-2016
  demo_i <- read.xport("raw_data/DEMO_I.XPT")
  write_rds(demo_i, "rds_data/demo_i")  

  
  
  
## Blood Pressure Measurements - 
  
  ## 2017-2018
  bpx_j <- read.xport("raw_data/BPX_J.XPT")
  write_rds(bpx_j, "rds_data/bpx_j")
  
  ## 2015-2016
  bpx_i <- read.xport("raw_data/BPX_I.XPT")
  write_rds(bpx_i, "rds_data/bpx_i")
  
  
  
  
## Blood Pressure Questionnaire -
  
  ## 2017-2018
  bpq_j <- read.xport("raw_data/BPQ_J.XPT")
  write_rds(bpq_j, "rds_data/bpq_j")

  ## 2015-2016
  bpq_i <- read.xport("raw_data/BPQ_I.XPT")
  write_rds(bpq_i, "rds_data/bpq_i")  

  
  
  
## Income -
  
  ## 2017-2018
  inq_j <- read.xport("raw_data/INQ_J.XPT")
  write_rds(inq_j, "rds_data/inq_j")
  
  ## 2015-2016
  inq_i <- read.xport("raw_data/INQ_I.XPT")
  write_rds(inq_i, "rds_data/inq_i")
  