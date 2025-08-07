library(tidyverse)
## In this script, we will determine **

df <- read.csv('DATA/facilties_national_2023_no_special_fac.csv')

# define function that means "not in" 
`%!in%` <- Negate(`%in%`)

# to be on the conservative side with respect to potentially poor data, remove facilities with 4 or fewer residents 
df <- df %>%
  filter(total_residents_dec_31 > 4) %>%
  filter(!is.na(Provider.Name))

# select the variables of interest: facility ID, % of residents with SMI
df_SMI <- df %>% select(a0100b_cms_crtfctn_num, CALC_SMI_per_resident, Ownership.Type, total_residents_dec_31)

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
  # remove homes for which SMI data is not available 
  na.omit(CALC_SMI_per_resident)

NH_abuse4 <- NH_abuse3 %>%
  mutate(
    `Percent of residents with SMI` = cut(
      CALC_SMI_per_resident,
      breaks = seq(0, 1, by = 0.1),
      include.lowest = TRUE,
      labels = paste(seq(0, 0.9, by = 0.1), "-", seq(0.1, 1, by = 0.1), sep = "")
    )
  ) %>%
  mutate(Ownership = case_when(str_detect(Ownership.Type,"Non") ~ "Non-profit",
                               str_detect(Ownership.Type, "Government") ~ "Government",
                               str_detect(Ownership.Type, "For") ~ "For-profit")) %>%
  mutate(Ownership = case_when(Ownership == 'For-profit' ~'For-profit',
                               Ownership %in% c('Non-profit', 'Government') ~ 'Non-profit or Gov.')) %>% 
  group_by(`Percent of residents with SMI`, Ownership) %>%
  # calculate number of homes, incidents with abuse, and avg abuse incidents per home, abuse per 100 residents  
  summarise(`Number of nursing homes` = n(), 
            number_incidents_w_abuse_sum = sum(number_abuse), 
            number_residents = sum(total_residents_dec_31),
            avg_abuse_incidents_per_home = number_incidents_w_abuse_sum/`Number of nursing homes`,
            avg_abuse_incidents_per_100_residents = number_incidents_w_abuse_sum/number_residents*100,
            std = stats::sd(number_abuse, na.rm = FALSE),
            min_abuse = min(number_abuse),
            max_abuse = max(number_abuse)) %>%
  na.omit(`Percent of residents with SMI`)


ggplot(NH_abuse4, aes(x = `Percent of residents with SMI`, y = avg_abuse_incidents_per_100_residents, size = `Number of nursing homes`, col = Ownership)) + 
  geom_point(alpha = 0.7) + 
  #scale_color_manual(values = c('firebrick2')) + 
  #scale_size_continuous(range  = c(1, 10), 
   #                     limits = c(1, 3), 
    #                    breaks = c(2)) +
  theme_ipsum_rc() + 
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

#at every step, for-proft homes have a higher abuse rate 
