library(tidyverse)
library(ggplot2)

## In this script, we will determine if SMI and staffing rates each have an independent effect on abuse frequency in nursing homes

df <- read.csv('DATA/facilties_national_2023_no_special_fac.csv')

# define function that means "not in" 
`%!in%` <- Negate(`%in%`)

# to be on the conservative side with respect to potentially poor data, remove facilities with 4 or fewer residents 
df <- df %>%
  filter(total_residents_dec_31 > 4) %>%
  filter(!is.na(Provider.Name)) 

# select the variables of interest: facility ID, % of residents with SMI
df_SMI <- df %>% select(a0100b_cms_crtfctn_num, CALC_SMI_per_resident,Reported.Total.Nurse.Staffing.Hours.per.Resident.per.Day)

# pull in nursing home data from CMS that includes citations as of February 2025 
# obtained here: https://data.cms.gov/provider-data/archived-data/nursing-homes
NH_info <- read_csv('INPUT_DATA/nursing_homes_including_rehab_services_02_2025/NH_HealthCitations_Feb2025.csv')

# select just abuse-related citations 
NH_abuse <- NH_info %>%
  filter(`Deficiency Category` == 'Freedom from Abuse, Neglect, and Exploitation Deficiencies') 

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
  select(`CMS Certification Number (CCN)`, number_abuse) %>%
  unique()

# join abuse dataset with SMI dataset
NH_abuse3 <- NH_abuse2 %>%
  full_join(df_SMI, join_by(`CMS Certification Number (CCN)` == a0100b_cms_crtfctn_num)) %>%
  # if, after joining, the number of abuse citations is NA, that means the facility wasn't in the abuse dataset, which means the number of citations should be zero
  mutate(number_abuse = ifelse(is.na(number_abuse), 0, number_abuse)) %>%
  # remove homes for which SMI or nurse hours data is not available 
  na.omit(CALC_SMI_per_resident, Reported.Total.Nurse.Staffing.Hours.per.Resident.per.Day)

# here is a linear regression to test whether SMI and staffing rates each have an independent effect on abuse
# the output of this is in the console, as well as with an explanatory write up here: 
# https://apmg.sharepoint.com/:w:/r/teams/APMResearchLab/Elisabeth/NURSING_HOMES/ANALYSIS/OTHER_FILES/testing%20independence%20of%20SMI%20and%20staffing%20on%20abuse%20frequency.docx?d=wbc7975d4d75e413086e162147d2f4d95&csf=1&web=1&e=MZGTKX
reg = lm(formula = number_abuse ~ CALC_SMI_per_resident + Reported.Total.Nurse.Staffing.Hours.per.Resident.per.Day, data = NH_abuse3)
summary(reg)

# to visualize the relationships, although this doesn't test their independence, two plots (also available in word doc linked above)
# first with SMI
ggplot(NH_abuse3, aes(x = CALC_SMI_per_resident, y = number_abuse))+ 
  geom_point()+
  geom_smooth(method = lm, formula = y ~ x)

# next with nursing staff
ggplot(NH_abuse3, aes(x = Reported.Total.Nurse.Staffing.Hours.per.Resident.per.Day, y = number_abuse))+ 
  geom_point()+
  geom_smooth(method = lm) 

# there is also a tercile-based analysis that demonstrates this relationship. That code is available in the analysis for the related graphic (#2).
