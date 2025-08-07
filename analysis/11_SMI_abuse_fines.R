library(tidyverse)
library(ggplot2)

## In this script, we will determine how often nursing homes cited for abuse and levied a fine by the federal government. 

df <- read.csv('DATA/facilties_national_2023_no_special_fac.csv')

# define function that means "not in" 
`%!in%` <- Negate(`%in%`)

# to be on the conservative side with respect to potentially poor data, remove facilities with 4 or fewer residents 
df <- df %>%
  filter(total_residents_dec_31 > 4) %>%
  filter(!is.na(Provider.Name))

# select the variables of interest: facility ID, % of residents with SMI
df_SMI <- df %>% select(a0100b_cms_crtfctn_num, CALC_SMI_per_resident)

# pull in nursing home data from CMS that includes citations as of February 2025 
# obtained here: https://data.cms.gov/provider-data/archived-data/nursing-homes
NH_info <- read_csv('INPUT_DATA/nursing_homes_including_rehab_services_02_2025/NH_HealthCitations_Feb2025.csv')

# select just abuse-related citations with a scope and severity code indicated actual harm occurred (assuming these are more likely worthy of resulting in a fine)
# see key of codes here: https://apmg.sharepoint.com/:b:/r/teams/APMResearchLab/Elisabeth/NURSING_HOMES/ANALYSIS/OTHER_FILES/SFFSCORINGMETHODOLOGY.pdf?csf=1&web=1&e=Onn2QO
NH_abuse <- NH_info %>%
  filter(`Deficiency Category` == 'Freedom from Abuse, Neglect, and Exploitation Deficiencies') %>%
  filter(`Scope Severity Code` %in% c('G', 'H', 'I', 'J', 'K', 'L'))



# test number of citations per year and month to see which years are consistent for use 
library(lubridate)
NH_abuse$year <- year(NH_abuse$`Survey Date`)
table(NH_abuse$year)
NH_abuse$MY <- format(NH_abuse$`Survey Date`, "%Y-%m") 
table(NH_abuse$MY)
# after looking at data from a few different years, it looks like they may begin to eliminate records after 3 years. So have a period of about 2 years 9 months with solid data. will take the window centered on 2023 since that is when our SMI data is from. 


# find number of abuse citations per facility, select CMS ID and number of abuses
NH_abuse2 <- NH_abuse %>%
  filter(`Survey Date` >= "2022-02-01") %>%
  filter(`Survey Date` < "2024-11-01") %>%
  group_by(`CMS Certification Number (CCN)`) %>%
  mutate(number_abuse = n()) %>%
  select(`CMS Certification Number (CCN)`, number_abuse, `Survey Date`, `Provider Name`, `Provider Address`, `City/Town`, State, `ZIP Code`) %>%
  unique()

# pull in fines data 
NH_penalities <- read_csv('INPUT_DATA/nursing_homes_including_rehab_services_02_2025/NH_Penalties_Feb2025.csv') %>%
  rename(`Survey Date` = `Penalty Date`) %>%
  filter(`Survey Date` >= "2022-02-01") %>%
  filter(`Survey Date` < "2024-11-01") %>%
  select(-Location, -`Processing Date`) %>%
  filter(`Penalty Type` != 'Payment Denial')

NH_abuse_and_penalities <- left_join(NH_abuse2, NH_penalities)

NH_abuse_and_penalities_summary <- NH_abuse_and_penalities %>%
  mutate(Fine.Amount = coalesce(`Fine Amount`, 0)) %>%
  mutate(Fine = ifelse(Fine.Amount >0, TRUE, FALSE)) %>%
  group_by(`CMS Certification Number (CCN)`) %>%
  summarise(number_incidents_fined = sum(Fine),
            number_incidents_w_abuse = n()) %>%
  mutate(percent_fined = number_incidents_fined/number_incidents_w_abuse*100)

#total
sum(NH_abuse_and_penalities_summary$number_incidents_fined)/sum(NH_abuse_and_penalities_summary$number_incidents_w_abuse)*100
# RESULT --> 66% of incidents that include an abuse citation received a fine 


# join abuse dataset with SMI dataset
NH_abuse_and_penalities_SMI <- NH_abuse_and_penalities_summary %>%
  left_join(df_SMI, join_by(`CMS Certification Number (CCN)` == a0100b_cms_crtfctn_num)) %>%
  mutate(more_than_half_SMI = ifelse(CALC_SMI_per_resident >= 0.50, TRUE, FALSE)) %>%
  group_by(more_than_half_SMI) %>%
  summarise(number_incidents_fined = sum(number_incidents_fined),
            number_incidents_w_abuse = sum(number_incidents_w_abuse)) %>%
  mutate(percent_fined = number_incidents_fined/number_incidents_w_abuse*100)

View(NH_abuse_and_penalities_SMI)
# RESULT --> % of incidents fined is lower for facilities with more than 50% SMI residents 
