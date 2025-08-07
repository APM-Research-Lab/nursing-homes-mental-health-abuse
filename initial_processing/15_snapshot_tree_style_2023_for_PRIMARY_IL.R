library(tidyverse)
library(data.table)
library(lubridate)
# define function that means "not in" 
`%!in%` <- Negate(`%in%`)

## The dataset used in this analysis is called the  "Minimum Data Set" (MDS) from the U.S. Centers for Medicare and Medicaid Services (CMS), which was our source, obtained via a Freedom of Information Act request. The data we requested and received covered the years 2013 through 2023. The MDS is based on information CMS receives from nursing homes that are eligible to receive funding from Medicare or Medicaid. Almost all nursing homes in the US receive funding from one of those entities. Thus, this data contains information for nearly all nursing homes. 

## Each row of the data is an assessment that has been conducted for a nursing home resident. These assessments are required periodically. The most comprehensive are when a resident is admitted, discharged, or on the annual anniversary of their admittance. There are also quarterly assessments. 

## This file takes all of assessments in the dataset and winnows them down to latest-dated assessment that contains a diagnosis of the resident's **primary medical condition**. Not all states collect this information.  The script will combine this back with the full census obtained via Script 1 in order to ensure enough data availability to make a conclusion. This data is being selected for an Illinois-specific analysis conducted in another script.

# define file name 
file <- "/Users/egawthrop/Library/CloudStorage/OneDrive-MinnesotaPublicRadio/MDS_DATA_2024/072022mds_Y2023.csv"

# read in file
df_original <- setDT(fread(file, header = T, colClasses = list(character = 'rsdnt_intrnl_id')))

# how many people in the full dataset 
length(unique(df_original$rsdnt_intrnl_id))

## select only the variables we will use for this story: 
# CMS ID number; what kind of federal OBRA assessment it is; whether and what kind of entry or discharge code; diagnosis codes for alzeimhers, dementia, manic depression (bipolar), psychotic disorder, schizophrenia; first other icd code; IDs for state, facility and individual; resident age; date the resident entered the facility 

# And again, we are only concerned with IL
df <- df_original %>% select(a0100b_cms_crtfctn_num, a0310a_fed_obra_cd, a0310f_entry_dschrg_cd, i0020b_prmry_mdcl_icd_cd, fac_prvdr_intrnl_id, c_rsdnt_age_num, rsdnt_intrnl_id, state_cd, trgt_dt, a1600_entry_dt) %>%
  filter(state_cd == 'IL')


#transform date to date data type   
df[, date := as.character(trgt_dt)]
df[, date := as.Date(date, format = '%d%b%Y')]

#make column with year 
df[, year := year(date)]

#make copy with all data for later 
df_all <- df

# how many assessments and individuals in the full dataset 
length(df_all$rsdnt_intrnl_id)
## --> 814261 assessments
length(unique(df_all$rsdnt_intrnl_id))
## --> 158003 unique people


## create true unique resident IDs by combining existing IDs and adding state code 
df[, rsdnt_intrnl_id := paste0(rsdnt_intrnl_id, '-', state_cd)]

# quick check for any funky CMS IDs
df_fac <- data.frame(table(df$a0100b_cms_crtfctn_num))

# there are some blanks - but the primary medical condition is also blank for those fields, so they will get removed further down 

# retain only unique observations (i.e. remove full duplicates)
df <- unique(df)

# how many assessments and individuals in the full dataset with simple clean
length(df$rsdnt_intrnl_id)
## --> 16,638,513 assessments
length(unique(df$rsdnt_intrnl_id))
## --> 3,617,621 unique people


## Now we begin the process to winnow down our assessments so that we achieve a census of those in a nursing home on Dec 31, 2023

# find people whose final date is discharge, and therefor are not in a nursing home on 12/31 
df_final_date_discharge <- df %>%
  group_by(rsdnt_intrnl_id) %>%
  # find the latest-dated assessment using max
  filter(date == max(date)) %>%
  # retain only records where that latest-dated assessment is a discharge code 
  # see code meanings here: https://apmg.sharepoint.com/:x:/r/teams/APMResearchLab/Elisabeth/NURSING_HOMES/ANALYSIS/OTHER_FILES/MDS%20FIELDS%20FOIA_06_04_24.xlsx?d=w10505c3a837e44d09867c89e2ccb2330&csf=1&web=1&e=QnmNM9
  filter(a0310f_entry_dschrg_cd %in% c(10, 11, 12))

# how many assessments and individuals in the discharged set 
length(df_final_date_discharge$rsdnt_intrnl_id)
## --> 2,405,634 assessments
length(unique(df_final_date_discharge$rsdnt_intrnl_id))
## --> 2400,181 unique people

# remove all assessments of those whose final assessment is a discharge
df_not_discharged <- df %>%
  filter(rsdnt_intrnl_id %!in% df_final_date_discharge$rsdnt_intrnl_id) 

# how many assessments and individuals in the assessments of those not discharged
length(df_not_discharged$rsdnt_intrnl_id)
## --> 7,015,583 assessments
length(unique(df_not_discharged$rsdnt_intrnl_id))
## --> 1,217,440 unique people


#convert blanks to NA for primary medical condition data and then remove NAs
df_not_discharged_NO_NAs <- df_not_discharged  %>% mutate(i0020b_prmry_mdcl_icd_cd = 
                      ifelse(i0020b_prmry_mdcl_icd_cd == "" | i0020b_prmry_mdcl_icd_cd == "^", NA, i0020b_prmry_mdcl_icd_cd)) %>%
  filter(!is.na(i0020b_prmry_mdcl_icd_cd))


# how many assessments and unique people in this dataset 
length(df_not_discharged_NO_NAs$rsdnt_intrnl_id)
## --> 234559 assessments
length(unique(df_not_discharged_NO_NAs$rsdnt_intrnl_id))
## --> 58961 unique people

# get distinct (though doesn't change anything)
distinct_not_discharged <- df_not_discharged_NO_NAs%>% 
  distinct(across(a0100b_cms_crtfctn_num:year))

## Like those discharged, some people have more than one entry on their latest assessment's date
# I am going to get rid of people where those latest assessments are conflicting with each other. 

# Define columns to check 
cols_to_check <- c('rsdnt_intrnl_id', 'date', 'i0020b_prmry_mdcl_icd_cd')


# 1182007 with only max date's assessment (but includes more than one max date when the diagnosis don't match)
dedupe = distinct_not_discharged %>%
  group_by(rsdnt_intrnl_id) %>% 
  filter(date==max(date)) %>%
  distinct(across(cols_to_check), .keep_all = TRUE)

# which people have more than one max date where the diagnosis don't match?

mismatch = dedupe %>% 
  group_by(rsdnt_intrnl_id) %>% 
  mutate(num = n()) %>% 
  filter(num > 1)


length(unique(mismatch$rsdnt_intrnl_id))
# 2-- residents where diagnosis don't match on max date --> omit them

deduped = dedupe %>% 
  filter(rsdnt_intrnl_id %!in% mismatch$rsdnt_intrnl_id)
# brings us to 58761

# get unique person, facility IDs
mismatch_fac <- mismatch %>% 
  select(rsdnt_intrnl_id, a0100b_cms_crtfctn_num) %>%
  unique()

## see if any hotspots of IDs_to_exclude 
# number with bad data by facility 
number_bad_by_fac <- data.frame(table(mismatch$a0100b_cms_crtfctn_num)) %>%
  rename(num_removed = Freq)

# number in final data by facility 
number_incl_by_fac <- data.frame(table(deduped$a0100b_cms_crtfctn_num)) %>%
  rename(num_census = Freq)

# percent missing in final data 
joined_number_by_fac <- left_join(number_bad_by_fac, number_incl_by_fac) %>%
  mutate(percent_missing = num_removed/(num_removed+num_census)*100)

# just those with 15% or more missing 
missing_15_pct <- joined_number_by_fac %>%
  filter(percent_missing >= 15)

# remove those with 15% or more missing 
all_residents_final_PRIMARY <- deduped %>% filter(a0100b_cms_crtfctn_num %!in% missing_15_pct$Var1)


## save in case wanted later
write.csv(all_residents_final_PRIMARY, 'DATA/IL_2023_PRIMARY.csv')

# now pull in other census data of all residents 
all_residents_final <- read.csv('Data/all_residents_final_national_2023.csv')  %>%
  filter(state_cd == 'IL')

# with primary data, remove date because primary data point may not have come from final dated assessment 
# still matching on same resident & nursing home
all_residents_final_PRIMARY <- all_residents_final_PRIMARY %>%
  select(-date)

# only use primary data for people identified via census (already proven to have consistent data)
all_residents_census_final_w_primary <- left_join(all_residents_final, all_residents_final_PRIMARY)

write.csv(all_residents_census_final_w_primary, 'DATA/all_residents_final_IL_2023_WITH_PRIMARY.csv')


