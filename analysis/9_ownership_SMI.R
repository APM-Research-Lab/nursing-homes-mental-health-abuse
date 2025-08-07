library(tidyverse)

## In this script, we will determine the percentage of high smi facilities that are for profit 

df <- read.csv('DATA/facilties_national_2023_no_special_fac.csv') %>%
  filter(!is.na(Provider.Name)) %>%
  filter(total_residents_dec_31 > 4)

# define function that means "not in" 
`%!in%` <- Negate(`%in%`)

df_ownership <- df %>% mutate(Ownership = case_when(str_detect(Ownership.Type,"Non") ~ "Non-profit",
                                                    str_detect(Ownership.Type, "Government") ~ "Government",
                                                    str_detect(Ownership.Type, "For") ~ "For-profit")) %>%
  mutate(high_SMI = case_when(CALC_SMI_per_resident >= 0.5 ~ TRUE, 
                              TRUE ~ FALSE))

# how many high SMI facilities ? 
high_SMI <- df_ownership %>%
  filter(high_SMI == TRUE)
# RESULT --> 494

# what % of high SMI facilities are for profit? 
df_ownership_high_SMI_facilities <- df_ownership %>%
  filter(high_SMI == TRUE) %>%
  group_by(Ownership) %>%
  summarise(number_ownership_type = n(),
            total = 516,
            percent_type = number_ownership_type/total*100)

# RESULT --> 88% 


# is that higher than for profit share of all facilities? 
df_ownership_all_facilities <- df_ownership %>%
  group_by(Ownership) %>%
  summarise(number_ownership_type = n(),
            total = length(df_ownership$a0100b_cms_crtfctn_num),
            percent_type = number_ownership_type/total*100)

# RESULT -> for profit is 71%, so yes 