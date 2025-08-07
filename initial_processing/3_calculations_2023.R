library(tidyverse)
year=2023

# define function that means "not in" 
`%!in%` <- Negate(`%in%`)

# load in the facilities with special oversight
fac_to_remove <- read_csv('DATA/facilities_to_remove.csv')

# load in resident census and remove facilities with special oversight 
all_residents_final <- read_csv(file = 'DATA/all_residents_final_national_2023.csv') %>% filter(a0100b_cms_crtfctn_num %!in% fac_to_remove$x) 

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
write.csv(all_residents_final, "DATA/national_snapshot_2023_w_SMI_no_special_fac.csv")

## CALCULATIONS BY FACILITY           
all_residents_final_grouped_summed2 <- all_residents_final %>%   
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
            # get total number of residents in census for the year 
            total_residents_dec_31 = n(),
            # calculate % of residents with our definition of SMI
            CALC_SMI_per_resident = CALC_SMI/total_residents_dec_31,
            # calculate average age of residents at facility 
            average_age = median(c_rsdnt_age_num),
            # calculate percent of residents under 65 at facility
            percent_below_65 = sum(age_under_65)/sum(n())*100) %>%
  # save year in column 
  mutate(year = year)


# combine with more data available from CMS
#CMS_file <- paste0('INPUT_DATA/NH_ProviderInfo_' , year, '.csv')
CMS_file_new <- 'INPUT_DATA/NH_ProviderInfo_Nov2023.csv'
cms_list <- read_csv(CMS_file_new) %>% select(1:12, 25, 39:45)
all_residents_SMI_totals_CMS_list <- left_join(all_residents_final_grouped_summed2, cms_list,
                                               by = c("a0100b_cms_crtfctn_num" = "CMS Certification Number (CCN)"))

# write facilities-level file 
write_csv(all_residents_SMI_totals_CMS_list, 'DATA/facilties_national_2023_no_special_fac.csv')

