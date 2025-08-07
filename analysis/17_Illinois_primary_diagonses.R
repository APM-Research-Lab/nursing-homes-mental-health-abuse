library(tidyverse)

# This script finds the number of facilities in Illinois where at least half of residents have a primary diagnosis of mental illness. It also calculates the number for North Aurora Care Center. 

df_IL <- read.csv('DATA/facilties_IL_with_primary_MI.csv')

df_IL2 <- df_IL %>% filter(total_residents_dec_31 > 4) %>%
  filter(!is.na(Provider.Name))

df_IL3 <- df_IL2 %>%
  filter(primary_MI_per_resident >= 0.50)

length(df_IL3$a0100b_cms_crtfctn_num)
# RESULT --> 16 facilities where at least half of the residents are primarily there due to a mental illness

df_IL4 <- df_IL2 %>%
  filter(primary_SMI_per_resident >= 0.50)
length(df_IL4$a0100b_cms_crtfctn_num)
# RESULT --> 15 facilities where at least half of the residents are primarily there due to a 'serious' mental illness

## North Aurora specifically 
north_aurora <- df_IL %>%
  filter(a0100b_cms_crtfctn_num == '14E306')
# RESULT --> 71%

# for comparison of facilities where at least 50% of residents have an SMI (primary or not)
df_all_SMI <- df_IL2 %>%
  filter(CALC_SMI_per_resident >= 0.5)
