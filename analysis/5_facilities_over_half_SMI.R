library(tidyverse)

# define function that means "not in" 
`%!in%` <- Negate(`%in%`)

## In this script, we will determine the number of facilities nationwide where at least half of the residents have a serious mental illness diagnosis. 


df <- read.csv('DATA/facilties_national_2023_no_special_fac.csv')

# select just facilities SMI rate is 50% or more 
fac_above_50 <- df %>%
  filter(CALC_SMI_per_resident >= 0.5)

# to be on the conservative side with respect to potentially poor data, remove facilities with 4 or fewer residents or where provider info not available

fac_above_50 <- fac_above_50 %>%
  filter(total_residents_dec_31> 4) %>%
  filter(!is.na(Provider.Name))

length(fac_above_50$a0100b_cms_crtfctn_num)
# RESULT --> 494 facilities 