library(tidyverse)
# define function that means "not in" 
`%!in%` <- Negate(`%in%`)

#This script defines mental illness from the diagnosis codes and then calculates and summarizes data at the facility, as in script 3. 

year = 2023
df <- read.csv('DATA/all_residents_final_IL_2023_WITH_PRIMARY.csv')

df_primary_info <- df %>%
  mutate(PRI_AVAILABLE = ifelse(is.na(i0020b_prmry_mdcl_icd_cd), FALSE, TRUE))

#check if anything isn't an ICD code
ICDs <- data.frame(table(df_primary_info$i0020b_prmry_mdcl_icd_cd, useNA = 'ifany'))

# select just IL 
df_primary_info_IL <- df_primary_info %>%
  filter(state_cd == 'IL')

# test data availability 
sum(df_primary_info_IL$PRI_AVAILABLE)/length(df_primary_info_IL$rsdnt_intrnl_id)
# RESULT --> 99.5% availability 

# group by facility and get % in facility with primary code that is a mental illness 

# there are no IL facilties to remove 
## read in ICD key - for reference, not used in code. these are illness categories. I broke down the "F" categories in more detail so as to include only relevant mental illnesses as defined for purposes of this story
ICD <- readxl::read_excel('INPUT_DATA/ICD_key.xlsx', sheet = 2)

df_primary_info_IL <- df_primary_info_IL %>% 
  mutate(ICD_primary_code = str_extract(i0020b_prmry_mdcl_icd_cd, "^.{2}")) %>%
  # primary SMI = schiz, bipolar, psychotic, and depressive disorders 
  mutate(primary_SMI = ifelse(ICD_primary_code %in% c('F2', 'F3'), TRUE, FALSE)) %>%
  # adds in anxiety and mood disorders 
  mutate(primary_MI = ifelse(ICD_primary_code %in% c('F2', 'F3', 'F4','F6'), TRUE, FALSE))

### CALCULATIONS 
## calculate the number of people with SMI 
# first convert relevant columns from characters to numbers 
#Bipolar
df_primary_info_IL$i5900_mnc_dprsn_cd <- as.numeric(df_primary_info_IL$i5900_mnc_dprsn_cd)
#Psychotic disorder
df_primary_info_IL$i5950_psychtc_cd <- as.numeric(df_primary_info_IL$i5950_psychtc_cd)
#Schizophrenia 
df_primary_info_IL$i6000_schzoprnia_cd <- as.numeric(df_primary_info_IL$i6000_schzoprnia_cd)

# create new column 'SMI' that uses a 1 to signify that someone has bipolar, schizophrenia,
# and/or psychotic disorder - this first one is the main metric used in story (Serious Mental Illness = SMI)
df_primary_info_IL <- df_primary_info_IL %>% 
  mutate(SMI = ifelse((i5900_mnc_dprsn_cd == 1 | i5950_psychtc_cd == 1 | i6000_schzoprnia_cd == 1), 1, 0))

# add column indicating if resident is under 65 
df_primary_info_IL <- df_primary_info_IL %>%
  mutate(age_under_65 = ifelse(c_rsdnt_age_num < 65, 1, 0))



## export all residents snapshot as csv 
write.csv(df_primary_info_IL, "DATA/IL_with_primary_MI.csv")


## CALCULATIONS BY FACILITY           
df_primary_info_IL_grouped_summed <- df_primary_info_IL %>%   
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
    primary_MI = sum(primary_MI, na.rm = TRUE),
    primary_SMI = sum(primary_SMI, na.rm = TRUE),
    # get total number of residents in census for the year
    total_residents_dec_31 = sum(n()),
    # calculate % of residents with our definition of SMI
    CALC_SMI_per_resident = CALC_SMI/total_residents_dec_31,
    # calculate % of residents with primary SMI
    primary_SMI_per_resident = primary_SMI/total_residents_dec_31,
    # calculate % of residents with primary MI
    primary_MI_per_resident = primary_MI/total_residents_dec_31,
    # calculate average age of residents at facility 
    average_age = median(c_rsdnt_age_num),
    # calculate percent of residents under 65 at facility
    percent_below_65 = sum(age_under_65)/sum(n())*100) %>%
  # save year in column 
  mutate(year = year)


CMS_file <- paste0('INPUT_DATA/NH_ProviderInfo_' , year, '.csv')
cms_list <- read_csv(CMS_file) %>% select(1:12, 25, 39:45)
df_primary_info_IL_SMI_totals_CMS_list <- left_join(df_primary_info_IL_grouped_summed, cms_list,
                                               by = c("a0100b_cms_crtfctn_num" = "Federal Provider Number"))

# write facilities-level file 
write_csv(df_primary_info_IL_SMI_totals_CMS_list, 'DATA/facilties_IL_with_primary_MI.csv')

