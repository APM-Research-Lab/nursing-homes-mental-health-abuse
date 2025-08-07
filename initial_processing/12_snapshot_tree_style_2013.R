library(tidyverse)
library(data.table)
library(lubridate)
# define function that means "not in" 
`%!in%` <- Negate(`%in%`)

## The dataset used in this analysis is called the  "Minimum Data Set" (MDS) from the U.S. Centers for Medicare and Medicaid Services (CMS), which was our source, obtained via a Freedom of Information Act request. The data we requested and received covered the years 2013 through 2023. The MDS is based on information CMS receives from nursing homes that are eligible to receive funding from Medicare or Medicaid. Almost all nursing homes in the US receive funding from one of those entities. Thus, this data contains information for nearly all nursing homes. 

## Each row of the data is an assessment that has been conducted for a nursing home resident. These assessments are required periodically. The most comprehensive are when a resident is admitted, discharged, or on the annual anniversary of their admittance. There are also quarterly assessments. 

## This file takes all of assessments in the dataset and winnows them down to latest-dated assessment that contains serious mental illness diagnosis data. It excludes anyone whose latest-dated assessment was a discharge, thus providing a census of residents on Dec 31, 2013. 

# This file will filter for just Illinois data, since that is the only state for which we include 2013 data in the story. 

# define file name 
file <- "/Users/egawthrop/Library/CloudStorage/OneDrive-MinnesotaPublicRadio/MDS_DATA_2024/072022mds_Y2013.csv"

# read in file
df_original <- setDT(fread(file, header = T, colClasses = list(character = 'rsdnt_intrnl_id')))


# how many people in the full dataset 
length(unique(df_original$rsdnt_intrnl_id))

## select only the variables we will use for this story: 
# CMS ID number; what kind of federal OBRA assessment it is; whether and what kind of entry or discharge code; diagnosis codes for alzeimhers, dementia, manic depression (bipolar), psychotic disorder, schizophrenia; first other icd code; IDs for state, facility and individual; resident age; date the resident entered the facility 
# Also here, filtering to just IL since we only use IL in story for 2013
df <- df_original %>% select(a0100b_cms_crtfctn_num, a0310a_fed_obra_cd, a0310f_entry_dschrg_cd, i5900_mnc_dprsn_cd, i5950_psychtc_cd,i6000_schzoprnia_cd, i8000a_icd_1_cd, fac_prvdr_intrnl_id, c_rsdnt_age_num, rsdnt_intrnl_id, state_cd, trgt_dt, a1600_entry_dt) %>%
  filter(state_cd == 'IL')


#transform date to date data type   
df[, date := as.character(trgt_dt)]
df[, date := as.Date(date, format = '%d%b%Y')]

#make column with year 
df[, year := year(date)]

# how many assessments and individuals in the full dataset 
length(df$rsdnt_intrnl_id)
## --> 20,800,657 assessments
length(unique(df$rsdnt_intrnl_id))
## --> 3,876,991 unique people

#convert blanks to 0 for mental illness diagnoses
df <- df %>% mutate(across(i5900_mnc_dprsn_cd:i6000_schzoprnia_cd, ~ifelse(. == "" | is.na(.), 0, .)))

## create true unique resident IDs by combining existing IDs and adding state code 
df[, rsdnt_intrnl_id := paste0(rsdnt_intrnl_id, '-', state_cd)]

## convert CMS IDs (see end of script for code showing justification)
df <- df %>%
  mutate(a0100b_cms_crtfctn_num = ifelse(fac_prvdr_intrnl_id == 5962, '145618', a0100b_cms_crtfctn_num),
         a0100b_cms_crtfctn_num = ifelse(fac_prvdr_intrnl_id == 5878, '146018', a0100b_cms_crtfctn_num),
         a0100b_cms_crtfctn_num = ifelse(fac_prvdr_intrnl_id == 5841, '14E306', a0100b_cms_crtfctn_num),
         a0100b_cms_crtfctn_num = ifelse(fac_prvdr_intrnl_id == 5756, '146156', a0100b_cms_crtfctn_num),
         a0100b_cms_crtfctn_num = ifelse(fac_prvdr_intrnl_id == 5616, '145515', a0100b_cms_crtfctn_num), 
         a0100b_cms_crtfctn_num = ifelse(fac_prvdr_intrnl_id == 5563, '14E847', a0100b_cms_crtfctn_num),
         a0100b_cms_crtfctn_num = ifelse(fac_prvdr_intrnl_id == 5518, '146164', a0100b_cms_crtfctn_num), 
         a0100b_cms_crtfctn_num = ifelse(fac_prvdr_intrnl_id == 5509, '146161', a0100b_cms_crtfctn_num),
         a0100b_cms_crtfctn_num = ifelse(fac_prvdr_intrnl_id == 5487, '14E812', a0100b_cms_crtfctn_num), 
         a0100b_cms_crtfctn_num = ifelse(fac_prvdr_intrnl_id == 5431, '14E836', a0100b_cms_crtfctn_num))

## remove all other missing CMS ID data
df <- df %>%
   filter(a0100b_cms_crtfctn_num %!in% c('^', 'NEEDED', 'NONE'))

# retain only unique observations (i.e. remove full duplicates)
df <- unique(df)

# how many assessments and individuals in the full dataset with simple clean
length(df$rsdnt_intrnl_id)
## --> 20,367,712 assessments
length(unique(df$rsdnt_intrnl_id))
## --> 3876991 unique people


## Now we begin the process to winnow down our assessments so that we achieve a census of those in a nursing home on Dec 31, 2013

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
## -->2493755 assessments
length(unique(df_final_date_discharge$rsdnt_intrnl_id))
## -->2488324 unique people

# remove all assessments of those whose final assessment is a discharge
df_not_discharged <- df %>%
  filter(rsdnt_intrnl_id %!in% df_final_date_discharge$rsdnt_intrnl_id) 

# how many assessments and individuals in the assessments of those not discharged
length(df_not_discharged$rsdnt_intrnl_id)
## --> 8576130 assessments
length(unique(df_not_discharged$rsdnt_intrnl_id))
## --> 1388667 unique people

# identify and remove rows where ICD diagnosis information not available : an indication of poor data quality 
df_not_discharged_blankICDs_removed <- df_not_discharged %>%
  # first convert blank data to NA-type data
  mutate_at(c('i8000a_icd_1_cd'), ~na_if(., '')) %>%
  # filter out NA data 
  filter(!is.na(i8000a_icd_1_cd)) %>%
  unique() 

# how many assessments and unique people in this dataset 
length(df_not_discharged_blankICDs_removed$rsdnt_intrnl_id)
## --> 6935102 assessments
length(unique(df_not_discharged_blankICDs_removed$rsdnt_intrnl_id))
## --> 1345804 unique people

# get distinct (though doesn't change anything)
distinct_not_discharged <- df_not_discharged_blankICDs_removed %>% 
  distinct(across(a0100b_cms_crtfctn_num:year))

## Like those discharged, some people have more than one entry on their latest assessment's date
# I am going to get rid of people where those latest assessments are conflicting with each other. 

# Define columns to check 
cols_to_check <- c("i5900_mnc_dprsn_cd","i5950_psychtc_cd","i6000_schzoprnia_cd","rsdnt_intrnl_id",'date')


# 1346120 with only max date's assessment (but includes more than one max date when the diagnosis don't match)
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
# 316 residents where diagnosis don't match on max date --> omit them

deduped = dedupe %>% 
  filter(rsdnt_intrnl_id %!in% mismatch$rsdnt_intrnl_id)
# brings us to 1345488

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
all_residents_final2 <- deduped %>% filter(a0100b_cms_crtfctn_num %!in% missing_15_pct$Var1)


## save 
write.csv(all_residents_final2, 'DATA/all_residents_final_IL_2013_cleaned.csv')

# OTHER EXPLANATIONS
## this was used to check on people with facility marked as '000000'
# check out residents for whom the facility ID is 000000


# look for weird facilities 
facs <- data.frame(table(all_residents_final$a0100b_cms_crtfctn_num))

# there are a lot more of these than there were for the 2023 data

# so we are just going to focus on IL 

IL <- all_residents_final %>%
  filter(state_cd == 'IL')

facs_IL <- data.frame(table(IL$a0100b_cms_crtfctn_num))

# seems like ^ is the main weird facility to look at, but there are others too 
all_0s <-  IL %>%
             filter(a0100b_cms_crtfctn_num %in% c('^', 'NEEDED', 'NONE')) 
  
#do any ^ have associated CMS numbers for some observations? 
IL_any <- IL %>%
  mutate(keep = ifelse(fac_prvdr_intrnl_id %in% all_0s$fac_prvdr_intrnl_id & a0100b_cms_crtfctn_num %!in% c('^', 'NEEDED', 'NONE'), TRUE, FALSE)) %>%
  filter(keep == TRUE)

# how many ^ in each facility? 
all_0s_for_corrections <- all_0s %>%
  filter(fac_prvdr_intrnl_id %in% IL_any$fac_prvdr_intrnl_id)

table(all_0s_for_corrections$fac_prvdr_intrnl_id)

## NEXT STEP IS SORTING THESE OUT            
           # The data has two facility ID number fields: the CMS ID number and a unique ID number that CMS generates, which can be combined with state code to be truly unique (fac_prvdr_intrnl_id).
           
           #  There are 1,744 people associated with CMS ID of "^", "needed" or "none" in the data.

# 338 are in a facility where one of the observations does have a CMS ID. I looked into these, and saved notes and CMS IDs here: https://apmg.sharepoint.com/:x:/r/teams/APMResearchLab/Elisabeth/NURSING_HOMES/ANALYSIS/INPUT_DATA/data_clean_IL_2023.xlsx?d=w0204917386724d3fb37e1a496571e393&csf=1&web=1&e=ZNb4dP. I made sure that any CMS ID added was not associated with any other facility 

# Let's look at the make up of the other people with missing CMS ID

other_0s <- all_0s %>%
  filter(a0100b_cms_crtfctn_num %in% all_0s_for_corrections$a0100b_cms_crtfctn_num)

# number of fac
length(unique(other_0s$fac_prvdr_intrnl_id))

#24 facilities

#SMI stats
other_0s <- other_0s %>%
  mutate(CALC_SMI = ifelse((i5900_mnc_dprsn_cd == 1 | i5950_psychtc_cd == 1 | i6000_schzoprnia_cd == 1), 1, 0))

sum(other_0s$CALC_SMI)/length(other_0s$rsdnt_intrnl_id)
#= 74%
# SO, removing these will make our estimate of SMI residents in IL in 2013 lower. 

           #There are 5 other rows. These will be removed, along with the 68 in NY.
           
           
           
           