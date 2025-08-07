library(tidyverse)
library(data.table)
library(lubridate)
# define function that means "not in" 
`%!in%` <- Negate(`%in%`)

## The dataset used in this analysis is called the  "Minimum Data Set" (MDS) from the U.S. Centers for Medicare and Medicaid Services (CMS), which was our source, obtained via a Freedom of Information Act request. The data we requested and received covered the years 2013 through 2023. The MDS is based on information CMS receives from nursing homes that are eligible to receive funding from Medicare or Medicaid. Almost all nursing homes in the US receive funding from one of those entities. Thus, this data contains information for nearly all nursing homes. 

## Each row of the data is an assessment that has been conducted for a nursing home resident. These assessments are required periodically. The most comprehensive are when a resident is admitted, discharged, or on the annual anniversary of their admittance. There are also quarterly assessments. 

## This file takes all of assessments in the dataset and winnows them down to latest-dated assessment that contains serious mental illness diagnosis data. It excludes anyone whose latest-dated assessment was a discharge, thus providing a census of residents on Dec 31, 2023. 

# define file name 
file <- "/Users/egawthrop/Library/CloudStorage/OneDrive-MinnesotaPublicRadio/MDS_DATA_2024/072022mds_Y2023.csv"

# read in file
df_original <- setDT(fread(file, header = T, colClasses = list(character = 'rsdnt_intrnl_id')))

# how many people in the full dataset 
length(unique(df_original$rsdnt_intrnl_id))

## select only the variables we will use for this story: 
# CMS ID number; what kind of federal OBRA assessment it is; whether and what kind of entry or discharge code; diagnosis codes for alzeimhers, dementia, manic depression (bipolar), psychotic disorder, schizophrenia; first other icd code; IDs for state, facility and individual; resident age; date the resident entered the facility 
df <- df_original %>% select(a0100b_cms_crtfctn_num, a0310a_fed_obra_cd, a0310f_entry_dschrg_cd, i5900_mnc_dprsn_cd, i5950_psychtc_cd,i6000_schzoprnia_cd, i8000a_icd_1_cd, fac_prvdr_intrnl_id, c_rsdnt_age_num, rsdnt_intrnl_id, state_cd, trgt_dt, a1600_entry_dt)


#transform date to date data type   
df[, date := as.character(trgt_dt)]
df[, date := as.Date(date, format = '%d%b%Y')]

#make column with year 
df[, year := year(date)]

# how many assessments and individuals in the full dataset 
length(df$rsdnt_intrnl_id)
## --> 17,488,691 assessments
length(unique(df$rsdnt_intrnl_id))
## --> 3,617,621 unique people

#convert blanks to 0 for mental illness diagnoses
df <- df %>% mutate(across(i5900_mnc_dprsn_cd:i6000_schzoprnia_cd, ~ifelse(. == "" | is.na(.), 0, .)))

## create true unique resident IDs by combining existing IDs and adding state code 
df[, rsdnt_intrnl_id := paste0(rsdnt_intrnl_id, '-', state_cd)]

## convert CMS ID of 000000 for one identified NJ facility (see end of script for code showing justification)
df <- df %>%
  mutate(a0100b_cms_crtfctn_num = ifelse((fac_prvdr_intrnl_id == 6 & state_cd == 'NJ'), '315229', a0100b_cms_crtfctn_num))

## remove all other 000000s
df <- df %>%
  filter(a0100b_cms_crtfctn_num != '000000')

# retain only unique observations (i.e. remove full duplicates)
df <- unique(df)

# how many assessments and individuals in the full dataset with simple clean
length(df$rsdnt_intrnl_id)
## --> 16,645,480 assessments
length(unique(df$rsdnt_intrnl_id))
## --> 3,617,472 unique people


## Now we begin the process to winnow down our assessments so that we achieve a census of those in a nursing home on Dec 31, 2023

# find people whose final date is discharge, and therefor are not in a nursing home on 12/31 
df_final_date_discharge <- df %>%
  group_by(rsdnt_intrnl_id) %>%
  # find the latest-dated assessment using max
  filter(date == max(date)) %>%
  # retain only records where that latest-dated assessment is a discharge code 
  # see code meanings here: https://apmg.sharepoint.com/:x:/r/teams/APMResearchLab/Elisabeth/NURSING_HOMES/ANALYSIS/OTHER_FILES/MDS%20FIELDS%20FOIA_06_04_24.xlsx?d=w10505c3a837e44d09867c89e2ccb2330&csf=1&web=1&e=QnmNM9
  filter(a0310f_entry_dschrg_cd %in% c(10, 11, 12))

## Data quality check, not necessary to run for creating dataset: look at people who appear more than once 
# RUN THIS --> temp_df <- data.table(table(df_final_date_discharge$rsdnt_intrnl_id))
# RUN THIS --> length(temp_df$V1)
# RUN THIS --> temp_df2 <- temp_df %>% filter(N > 1)
# RUN THIS --> length(temp_df2$V1)
## Of 2,400,181 people whose final date is a discharge, 5,951 appear more than once (i.e. had multiple entries, indicating an error). That's 0.25%. 
## Looked up some ID numbers from temp_df that appear more than once to understand where errors are.
## Looks like differences in diagnosis codes, esp the ICD1 code, or for example has a 10, 11 and 12 discharge assessment coded on the same day. 

# how many assessments and individuals in the discharged set 
length(df_final_date_discharge$rsdnt_intrnl_id)
## --> 2,406,024 assessments
length(unique(df_final_date_discharge$rsdnt_intrnl_id))
## --> 2,400,104 unique people

# remove all assessments of those whose final assessment is a discharge
df_not_discharged <- df %>%
  filter(rsdnt_intrnl_id %!in% df_final_date_discharge$rsdnt_intrnl_id) 

# how many assessments and individuals in the assessments of those not discharged
length(df_not_discharged$rsdnt_intrnl_id)
## --> 7,027,519 assessments
length(unique(df_not_discharged$rsdnt_intrnl_id))
## --> 1,217,368 unique people

# identify and remove rows where ICD diagnosis information not available : an indication of poor data quality 
df_not_discharged_blankICDs_removed <- df_not_discharged %>%
  # first convert blank data to NA-type data
  mutate_at(c('i8000a_icd_1_cd'), ~na_if(., '')) %>%
  # filter out NA data 
  filter(!is.na(i8000a_icd_1_cd)) %>%
  unique() 

# how many assessments and unique people in this dataset 
length(df_not_discharged_blankICDs_removed$rsdnt_intrnl_id)
## --> 5,455,209 assessments
length(unique(df_not_discharged_blankICDs_removed$rsdnt_intrnl_id))
## --> 1,178,730 unique people

# get distinct (though doesn't change anything)
distinct_not_discharged <- df_not_discharged_blankICDs_removed %>% 
  distinct(across(a0100b_cms_crtfctn_num:year))

## Like those discharged, some people have more than one entry on their latest assessment's date
# I am going to get rid of people where those latest assessments are conflicting with each other. 

# Define columns to check 
cols_to_check <- c("i5900_mnc_dprsn_cd","i5950_psychtc_cd","i6000_schzoprnia_cd","rsdnt_intrnl_id",'date')


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
# 3265 residents where diagnosis don't match on max date --> omit them

cleaned = dedupe %>% 
  filter(rsdnt_intrnl_id %!in% mismatch$rsdnt_intrnl_id)


# how many assessments and unique people in this dataset 
length(cleaned$rsdnt_intrnl_id)
## --> 1175465 assessments
length(unique(cleaned$rsdnt_intrnl_id))
## --> 1175465 unique people

# # get unique person, facility IDs
# mismatch_fac <- mismatch %>% 
#   select(rsdnt_intrnl_id, a0100b_cms_crtfctn_num) %>%
#   unique()

## see if any hotspots of IDs to exclude - i.e. nursing homes that seem to not report data well 
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
all_residents_final <- deduped %>% filter(a0100b_cms_crtfctn_num %!in% missing_15_pct$Var1)

length(all_residents_final$rsdnt_intrnl_id)
## --> 1,170,324 assessments
length(unique(all_residents_final$rsdnt_intrnl_id))
## --> 1,170,324 unique people

## save 
write.csv(all_residents_final, 'DATA/all_residents_final_national_2023.csv')

## also save the 50 facilities that have a lot of missing data, for use later in map/lookup table 

missing_50 <- deduped %>% filter(a0100b_cms_crtfctn_num %in% missing_15_pct$Var1)
  
write.csv(missing_50, 'DATA/many_missing_residents_final_national_2023.csv')


# --------- # 
# END OF ANALYSIS SCRIPT # 

# OTHER EXPLANATIONS
## this was used to check on people with facility marked as '000000'
# check out residents for whom the facility ID is 000000

all_0s <- all_residents_final %>%
  filter(a0100b_cms_crtfctn_num == '000000')


# The data has two facility ID number fields: the CMS ID number and a unique ID number that CMS generates, which can be combined with state code to be truly unique.

#  There are 157 people associated with CMS ID of "000000" in the data.
# 84 come from a facility in NJ, where the state ID number is "6". For people over 22 at that facility, the CMS facility is #315229. For people 22 and udner, they use “000000" as the CMS ID number. https://www.medicare.gov/care-compare/details/nursing-home/315229/view-all?state=NJ. The young people are about half of the residents. We will change the CMS ID of those 22 and under to match that of those over 22. 


#Another 68 of the 000000s come from a facility in NY for which its internal ID doesn’t match with a CMS ID for any observation, so it’s impossible to say what facility this might be.

other_0s <- all_residents_final %>%
  filter(a0100b_cms_crtfctn_num == '000000') %>%
  mutate(drop = ifelse((fac_prvdr_intrnl_id == 2322 & state_cd == 'NY'), TRUE, FALSE),
         drop = ifelse((fac_prvdr_intrnl_id == 6 & state_cd == 'NJ'), TRUE, drop)) %>%
  filter(drop == FALSE)

#There are 5 other rows. These will be removed, along with the 68 in NY.


