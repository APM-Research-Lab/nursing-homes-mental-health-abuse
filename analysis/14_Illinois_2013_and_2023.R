library(tidyverse)

# define function that means "not in" 
`%!in%` <- Negate(`%in%`)

## In this script, we will compare the percent of nursing home residents in IL with SMI in 2013 and 2023

df_2013 <- read.csv('DATA/facilties_IL_2013.csv') %>% 
  filter(total_residents_dec_31 > 4)

## for 2013, should get rid of the IL IMDs. Have a list of the IMDs, but they don't have the CMS numbers. Will get those from Brown U data. 

library(readxl)
brown <- read_xls('INPUT_DATA/brown_facility_2013.xls')

# use to manually look up IDs for IMDs, add to this spreadsheet, and then read in: 
IL_IMDs_2013 <- read_excel('INPUT_DATA/IMDs_through_the_years.xlsx', sheet = '2013')

df_2013_minus_IMDs <- df_2013 %>%
  filter(a0100b_cms_crtfctn_num %!in% IL_IMDs_2013$CMS)

# 2013 calc - no IMDs 
sum(df_2013_minus_IMDs$CALC_SMI)/sum(df_2013_minus_IMDs$total_residents_dec_31)
# RESULT --> 22.4% 


## Now do with 'cleaned' data 
df_2013_cleaned <- read.csv('DATA/facilties_IL_2013_cleaned.csv') %>% 
  filter(total_residents_dec_31 > 4)

## for 2013, should get rid of the IL IMDs. 

df_2013_cleaned_minus_IMDs <- df_2013_cleaned %>%
  filter(a0100b_cms_crtfctn_num %!in% IL_IMDs_2013$CMS)

# 2013 calc - no IMDs and CMS IDs cleaned
sum(df_2013_cleaned_minus_IMDs$CALC_SMI)/sum(df_2013_cleaned_minus_IMDs $total_residents_dec_31)
# RESULT --> 21.1% 

# #NOW 2023 

df_2023 <- read.csv('DATA/facilties_national_2023_no_special_fac.csv') %>% filter(state_cd == 'IL') %>%
  filter(total_residents_dec_31 > 4)

# 2023 calc
sum(df_2023$CALC_SMI)/sum(df_2023$total_residents_dec_31)
# RESULT --> 25.7% 

# If only including data for which we have a CMS facility ID
25.7-21.1
# == 4.6 percentage points different 

# If also including data for which we DON'T have a CMS fac ID 
25.7-22.4
# == 3.3 percentage points different 
