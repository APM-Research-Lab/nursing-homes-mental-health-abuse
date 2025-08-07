library(tidyverse)

## In this script, we will determine how many people nationwide are in a nursing home with a serious mental illness.

## For this an other scripts, the facility-level data is all that is needed to pull in, not the resident-level. When possible, that is what will be done, since the resident-level is larger and slower. 

df <- read.csv('DATA/facilties_national_2023_no_special_fac.csv')

df2 <- df %>%
  filter(total_residents_dec_31> 4) %>%
  filter(!is.na(Provider.Name))

## get total number of residents with SMI
total_w_SMI <- sum(df2$CALC_SMI)
total_w_SMI
# RESULT => 216,718


## get total of census 
total_res <- sum(df2$total_residents_dec_31)
total_res
# RESULT => 1148540


## percent of residents with SMI 
total_w_SMI/total_res*100
# RESULT => 18.9%


