library(tidyverse)
year = 2023
# In this script we compare the number of abuse citations at facilities with and with out particular oversight in states where that exists 

# define function that means "not in" 
`%!in%` <- Negate(`%in%`)

# load in the facilities with special oversight
special_fac <- read_csv('DATA/facilities_to_remove.csv')

# load in resident census
all_residents_final <- read_csv(file = 'DATA/all_residents_final_national_2023.csv')

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
  # add column indicating if special facility or not 
  mutate(special_fac = ifelse(a0100b_cms_crtfctn_num %in% special_fac$x, TRUE, FALSE))


# combine with more data available from CMS
CMS_file <- paste0('INPUT_DATA/NH_ProviderInfo_' , year, '.csv')
cms_list <- read_csv(CMS_file) %>% select(1:12, 25, 39:45)
all_residents_SMI_totals_CMS_list <- left_join(all_residents_final_grouped_summed, cms_list,
                                               by = c("a0100b_cms_crtfctn_num" = "Federal Provider Number"))



df <- all_residents_SMI_totals_CMS_list %>%
  filter(!is.na(`Provider Name`)) %>%
  filter(total_residents_dec_31 > 4)

# pull in nursing home data that includes citations as of Feb 2025
# obtained here: https://data.cms.gov/provider-data/archived-data/nursing-homes 
NH_info <- read_csv('INPUT_DATA/nursing_homes_including_rehab_services_02_2025/NH_HealthCitations_Feb2025.csv')

# select just abuse-related citations, using all severity/scope levels since more limited sample 
NH_abuse <- NH_info %>%
  filter(`Deficiency Category` == 'Freedom from Abuse, Neglect, and Exploitation Deficiencies') %>%
  filter(`Survey Date` >= "2022-02-01") %>%
  filter(`Survey Date` < "2024-11-01") 


frequency_abuse <- NH_abuse %>% 
  group_by(`CMS Certification Number (CCN)`) %>% 
  mutate(total_citations_abuse = n()) %>%
  select(`CMS Certification Number (CCN)`, total_citations_abuse) %>% #, Provider.Name, Provider.Address, City.Town, State) %>%
  unique()

df_and_abuse <- left_join(df, frequency_abuse, by = join_by(a0100b_cms_crtfctn_num == `CMS Certification Number (CCN)`)) %>%
  # this converts NA in abuse column to 0 
  mutate(total_citations_abuse = coalesce(total_citations_abuse, 0))

# now get just states with 5 or more special facilities
states_not_enough_fac <- df %>% 
  group_by(state_cd, special_fac) %>% 
  summarise(number_fac = n()) %>%
  mutate(low_count = ifelse(number_fac < 5, TRUE, FALSE)) %>%
  filter(low_count == TRUE) %>%
  select(state_cd)

states_with_special <- df %>%
  filter(state_cd %!in% states_not_enough_fac$state_cd) %>%
  group_by(state_cd, special_fac) %>% 
  summarise(number_fac = n()) %>%
  filter(special_fac == TRUE)
#NV_fac2 <- df2 %>% filter(state_cd == 'NV')

df_all_state <- df_and_abuse %>% 
  filter(state_cd %in% states_with_special$state_cd) %>%
  group_by(state_cd, special_fac) %>%
  summarise(number_fac = n(), 
            total_SMI = sum(CALC_SMI),
            total_residents_dec_31 = sum(total_residents_dec_31),
            percent_SMI = total_SMI/total_residents_dec_31*100,
            mean_all_abuse = mean(total_citations_abuse)) %>%
  ungroup() %>%
  group_by(state_cd) %>%
  mutate(min = min(mean_all_abuse), 
         max = max(mean_all_abuse))

# Plot - both for help in interpreting data (although can also use (View(df_all_state))) and for the graphic 
library(hrbrthemes)
p<-ggplot(df_all_state) +
  geom_segment( aes(x=state_cd, xend=state_cd, y=min, yend=max), color="grey") +
  geom_point( aes(x=state_cd, y=mean_all_abuse, color=special_fac, size=number_fac)) +
  scale_color_manual(values = c('TRUE' = "#900101", 'FALSE' = "#1E65E5")) +
  scale_size_continuous(range = c(4, 15)) + # Default is usually c(1, 6) +
  coord_flip()+
  theme_ipsum() +
  theme(
    legend.position = "none",
  ) +
  labs(subtitle = "Dark red are the facilities with special oversight\nSize of circle proportional to number of facilities") + 
  xlab("") +
  ylab("Average number of abuse-related citations per facility")

library(svglite)
svglite("special_facilities.svg", width = 8, height = 6)
print(p)
dev.off()

df_box_plot <- df_and_abuse %>% 
  filter(state_cd %in% states_with_special$state_cd) %>%
  group_by(special_fac) %>%
  mutate(number_fac = n(), 
            total_SMI = sum(CALC_SMI),
            total_residents_dec_31 = sum(total_residents_dec_31),
            percent_SMI = total_SMI/total_residents_dec_31*100,
            mean_all_abuse = mean(total_citations_abuse),
            median_abuse = median(total_citations_abuse),
          p10 = quantile(total_citations_abuse, probs = 0.05, na.rm = TRUE),
         p90 = quantile(total_citations_abuse, probs = 0.95, na.rm = TRUE)
         ) %>%
  ungroup() 

df_new_plot <- df_and_abuse %>% 
  filter(state_cd %in% states_with_special$state_cd) %>%
  mutate(
  `Percent of residents with SMI` = cut(
    CALC_SMI_per_resident,
    breaks = seq(0, 1, by = 0.25),
    include.lowest = TRUE,
    labels = paste(seq(0, 0.75, by = 0.25), "-", seq(0.25, 1, by = 0.25), sep = "")
  )) %>%
  group_by(`Percent of residents with SMI`, special_fac) %>%
  summarise(`Number of nursing homes` = n(), 
              number_incidents_w_abuse_sum = sum(total_citations_abuse), 
              avg_abuse_incidents_per_home = number_incidents_w_abuse_sum/`Number of nursing homes`)

ggplot(df_new_plot, aes(x = `Percent of residents with SMI`, y = avg_abuse_incidents_per_home, size = 5, col =special_fac)) + 
  geom_point(alpha = 0.7) + 
  #scale_color_manual(values = c('firebrick2')) + 
  #scale_size_continuous(range  = c(1, 10), 
  #                     limits = c(1, 3), 
  #                    breaks = c(2)) +
  theme_ipsum_rc()


df_new_plot_states <- df_and_abuse %>% 
  filter(state_cd %in% states_with_special$state_cd) %>%
  mutate(
    `Percent of residents with SMI` = cut(
      CALC_SMI_per_resident,
      breaks = seq(0, 1, by = 0.5),
      include.lowest = TRUE,
      labels = paste(seq(0, 0.5, by = 0.5), "-", seq(0.5, 1, by = 0.5), sep = "")
    )) %>%
  group_by(`Percent of residents with SMI`, special_fac, state_cd) %>%
  summarise(`Number of nursing homes` = n(), 
            number_incidents_w_abuse_sum = sum(total_citations_abuse), 
            avg_abuse_incidents_per_home = number_incidents_w_abuse_sum/`Number of nursing homes`)


df_new_plot_states_select <- df_and_abuse %>% 
  filter(state_cd %in% states_with_special$state_cd)

ggplot(df_box_plot, aes(x = total_citations_abuse, y = percent_SMI, color = special_fac)) + 
  geom_boxplot(outliers = FALSE)

ggplot(df_box_plot, aes(x = total_citations_abuse, y = percent_SMI, color = special_fac)) + 
  geom_boxplot(outliers = TRUE)
