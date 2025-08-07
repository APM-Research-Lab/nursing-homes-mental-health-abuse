library(tidyverse)
year=2013

## Processing for facility-level data for 2013

# define function that means "not in" 
`%!in%` <- Negate(`%in%`)

# load in resident census 
all_residents_final <- read_csv(file = 'DATA/all_residents_final_IL_2013.csv') 

### CALCULATIONS 
## calculate the number of people with SMI 
# first convert relevant columns from characters to numbers 
#Bipolar
all_residents_final$i5900_mnc_dprsn_cd <- as.numeric(all_residents_final$i5900_mnc_dprsn_cd)
#Psychotic disorder
all_residents_final$i5950_psychtc_cd <- as.numeric(all_residents_final$i5950_psychtc_cd)
#Schizophrenia 
all_residents_final$i6000_schzoprnia_cd <- as.numeric(all_residents_final$i6000_schzoprnia_cd)

# create new column 'SMI' that uses a 1 to signify that someone has bipolar, schizophrenia,
# and/or psychotic disorder - this first one is the main metric used in story (Serious Mental Illness = SMI)
all_residents_final <- all_residents_final %>% 
  mutate(SMI = ifelse((i5900_mnc_dprsn_cd == 1 | i5950_psychtc_cd == 1 | i6000_schzoprnia_cd == 1), 1, 0))

# add column indicating if resident is under 65 
all_residents_final <- all_residents_final %>%
  mutate(age_under_65 = ifelse(c_rsdnt_age_num < 65, 1, 0))


## export all residents snapshot as csv 
write.csv(all_residents_final, "DATA/IL_snapshot_2013_w_SMI.csv")


## CALCULATIONS BY FACILITY           
all_residents_final_grouped_summed <- all_residents_final %>%   
  # group by facility ID number 
  group_by(a0100b_cms_crtfctn_num, state_cd) %>% 
  # calculations, starting with adding up the number of people with an SMI
  summarise(
    # sum people with SMI
    CALC_SMI = sum(SMI, na.rm = TRUE),
    # sum people with various SMI separately
    psychotic = sum(i5950_psychtc_cd, na.rm = TRUE),
    bipolar = sum(i5900_mnc_dprsn_cd, na.rm = TRUE),
    schiz = sum(i6000_schzoprnia_cd, na.rm = TRUE),
    # get total number of residents in census for the eyar 
    total_residents_dec_31 = sum(n()),
    # calculate % of residents with our definition of SMI
    CALC_SMI_per_resident = CALC_SMI/total_residents_dec_31,
    # calculate average age of residents at facility 
    average_age = median(c_rsdnt_age_num),
    # calculate percent of residents under 65 at facility
    percent_below_65 = sum(age_under_65)/sum(n())*100) %>%
  # save year in column 
  mutate(year = year)

# combine with more data available from CMS
# 2016 is earliest year available for metadata - but we don't actually use any provider names for this so probably don't even need this step, but keeping just in case useful for later reference for specific homes. 
year = 2016
CMS_file <- paste0('INPUT_DATA/NH_ProviderInfo_' , year, '.csv')
cms_list <- read_csv(CMS_file) %>% select(1:12, 25, 39:45)
all_residents_SMI_totals_CMS_list <- left_join(all_residents_final_grouped_summed, cms_list,
                                               by = c("a0100b_cms_crtfctn_num" = "provnum"))

# write facilities-level file 
write_csv(all_residents_SMI_totals_CMS_list, 'DATA/facilties_IL_2013.csv')


# NOW WITH DATA THAT EXCLUDES MISSING CMS DATA
# load in resident census 
all_residents_final <- read_csv(file = 'DATA/all_residents_final_IL_2013_cleaned.csv') 

### CALCULATIONS 
## calculate the number of people with SMI 
# first convert relevant columns from characters to numbers 
#Bipolar
all_residents_final$i5900_mnc_dprsn_cd <- as.numeric(all_residents_final$i5900_mnc_dprsn_cd)
#Psychotic disorder
all_residents_final$i5950_psychtc_cd <- as.numeric(all_residents_final$i5950_psychtc_cd)
#Schizophrenia 
all_residents_final$i6000_schzoprnia_cd <- as.numeric(all_residents_final$i6000_schzoprnia_cd)

# create new column 'SMI' that uses a 1 to signify that someone has bipolar, schizophrenia,
# and/or psychotic disorder - this first one is the main metric used in story (Serious Mental Illness = SMI)
all_residents_final <- all_residents_final %>% 
  mutate(SMI = ifelse((i5900_mnc_dprsn_cd == 1 | i5950_psychtc_cd == 1 | i6000_schzoprnia_cd == 1), 1, 0))

# add column indicating if resident is under 65 
all_residents_final <- all_residents_final %>%
  mutate(age_under_65 = ifelse(c_rsdnt_age_num < 65, 1, 0))


## export all residents snapshot as csv 
write.csv(all_residents_final, "DATA/IL_snapshot_2013_w_SMI_cleaned.csv")

## CALCULATIONS BY FACILITY           
all_residents_final_grouped_summed <- all_residents_final %>%   
  # group by facility ID number 
  group_by(a0100b_cms_crtfctn_num, state_cd) %>% 
  # calculations, starting with adding up the number of people with an SMI
  summarise(
    # sum people with SMI
    CALC_SMI = sum(SMI, na.rm = TRUE),
    # sum people with various SMI separately
    psychotic = sum(i5950_psychtc_cd, na.rm = TRUE),
    bipolar = sum(i5900_mnc_dprsn_cd, na.rm = TRUE),
    schiz = sum(i6000_schzoprnia_cd, na.rm = TRUE),
    # get total number of residents in census for the eyar 
    total_residents_dec_31 = sum(n()),
    # calculate % of residents with our definition of SMI
    CALC_SMI_per_resident = CALC_SMI/total_residents_dec_31,
    # calculate average age of residents at facility 
    average_age = median(c_rsdnt_age_num),
    # calculate percent of residents under 65 at facility
    percent_below_65 = sum(age_under_65)/sum(n())*100) %>%
  # save year in column 
  mutate(year = year)

# combine with more data available from CMS
# 2016 is earliest year available for metadata - but we don't actually use any provider names for this so probably don't even need this step, but keeping just in case useful for later reference for specific homes. 
year = 2016
CMS_file <- paste0('INPUT_DATA/NH_ProviderInfo_' , year, '.csv')
cms_list <- read_csv(CMS_file) %>% select(1:12, 25, 39:45)
all_residents_SMI_totals_CMS_list <- left_join(all_residents_final_grouped_summed, cms_list,
                                               by = c("a0100b_cms_crtfctn_num" = "provnum"))

# write facilities-level file 
write_csv(all_residents_SMI_totals_CMS_list, 'DATA/facilties_IL_2013_cleaned.csv')

