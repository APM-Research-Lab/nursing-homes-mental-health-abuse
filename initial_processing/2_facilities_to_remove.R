library(tidyverse)
library(readxl)

## This script is used to identify the CMS ID numbers for facilities that have specific oversight related to caring for people with SMI in nursing homes. We will be removing these facilities from our census dataset for most of our analyses, because we want to focus on those "regular" nursing homes that are under no particular special regulations. 

## The nursing homes in each state that are in some way under some specific oversight for SMI care were identified by contacting the health department or similar agency in each state. We did not get a response from all states. The summary of those responses is available here (should work with @mpr.org email addresses): https://airtable.com/invite/l?inviteId=invcSaVwX3PaXNEoh&inviteToken=a82d8e0ff843b99fd4eb2c3d3fdab20670b12f1d9a4d899944d0195d93a1d643


# First, get national provider info from CMS. This contains the CMS ID numbers as well as other identifiers we will try to match. 
provider_info <- read.csv('INPUT_DATA/NH_ProviderInfo_2023.csv', colClasses = 'character')

# 1st state - Colorado
# data was received via email from state
# notable: these include homes with secure environments for dementia OR mental illness, and they don't distinguish between the two. so we will just remove any home with a secure environment, to be on the 'generous' side. 

# read in colorado nursing homes 
colorado <- read_xlsx('INPUT_DATA/states_special_facilities/CO_Nursing Facilities - Secure Environments_03.26.2024.xlsx', skip = 7, col_types = 'text')

# join with national CMS info by facility name and zip code 
colorado_with_national_numbers <- left_join(colorado, provider_info, by=join_by(`Facility Name` == Provider.Name, Zip == Provider.Zip.Code))

# get just ID numbers and facility names 
colorado_IDs <- colorado_with_national_numbers$Federal.Provider.Number
colorado_names <- colorado_with_national_numbers$`Facility Name`

# 5 facilities from CO's dataset didn't have a match with a CMS ID. looked these up manually, saving IDs and names. Four of them are CMS-certified, one is not and thus is not included here. 
additional_CO_IDs <- c('065191', '065248', '065415', '065238')
additional_CO_names <- c('AUTUMN HEIGHTS POST ACUTE', 'BETHANY NURSING & REHAB CENTER', 'PIKES PEAK CENTER', 'ELMS HAVEN CENTER')

# 2nd state - Idaho 
# Homes with Behavioral Care Units listed on their website - looked up CMS ID for each. 
# website: https://healthandwelfare.idaho.gov/providers/idaho-medicaid-providers/medicaid-nursing-facilities 
idaho_IDs <- c('135093', '135014', '135051', '135146', '135048', '135052', '135069', '135125', '135089', '135011', '135056',
               '135076','135081', '135065', '135084', '135019', '135068', '135090', '135055', '135075', '135010')
idaho_names <- c('ASPEN PARK OF CASCADIA', 'CALDWELL CARE OF CASCADIA', 'CANYON WEST OF CASCADIA, LLC DBA CANYON WEST OF CA',
                 'CASCADIA OF BOISE', 'CLEARWATER HEALTH & REHABILITATION OF CASCADIA', 'COEUR D\'ALENE HEALTH & REHABILITATION OF CASCADIA',
                 'COVE OF CASCADIA, THE', 'CREEKSIDE TRANSITIONAL CARE AND REHABILITATION', 'DESERT VIEW CARE CENTER OF BUHL',
                 'GATEWAY TRANSITIONAL CARE CENTER', 'LINCOLN COUNTY CARE CENTER', 'MEADOW VIEW NURSING AND REHABILITATION',
                 'MINI-CASSIA CARE CENTER', 'MOUNTAIN VALLEY OF CASCADIA','OAK CREEK REHABILITATION CENTER OF KIMBERLY',
                 'ORCHARDS OF CASCADIA, THE', 'PARKE VIEW REHABILITATION & CARE CENTER', 'SHAW MOUNTAIN OF CASCADIA',
                 'VALLEY VISTA CARE CENTER OF SANDPOINT', 'VALLEY VISTA CARE CENTER OF ST MARIES', 'WEISER CARE OF CASCADIA')

# 3rd - Kansas
# list obtained via state website and confirmed via email 
# https://webapps.kdads.ks.gov/prod/f?p=113:901
kansas <- read.csv('INPUT_DATA/states_special_facilities/KS_Survey and Certification Commission .csv')

kansas_with_national_numbers <- kansas %>%
  #only facilities with the text "Mental" apply 
  filter(str_detect(Facility.Type, "Mental")) %>%
  # phone number seems to be an effective joiner with national data 
  mutate(Phone = str_remove_all(Phone, '-')) %>%
  left_join(provider_info, by=join_by(Phone == Provider.Phone.Number), keep = TRUE) %>%
  select(Facility.Name, Provider.Name, everything())

# Save IDs and names
kansas_IDs <- kansas_with_national_numbers$Federal.Provider.Number
kansas_names <- kansas_with_national_numbers$Facility.Name

# two facilities didn't match with an ID number. one is a test facility and the other is here:
additional_KS_IDs <- c('17E038')
additional_KS_names <- c('HAVILAND OPERATOR LLC')

# 4th Nevada
# List received from state via email
# I matched some facilities but easier to look up by hand and then input into excel 
nevada <- read_xls('INPUT_DATA/states_special_facilities/NV_BCCP_Providers.xls')

nevada_IDs <- nevada$CMS
nevada_names <- nevada$`Provider Name`

#5th New Jersey 
# Names received via email from state, IDs looked up. 
new_jersey_IDs <- c('315244', '315376', '315280', '315303', '315361', '315357', '315221')
new_jersey_names <- c('PREFERRED CARE AT ABSECON', 'CHRISTIAN HEALTH CARE CENTER', 'SILVER HEALTHCARE CENTER',
                      'MORRIS VIEW HEALTHCARE CENTER', 'PREAKNESS HEALTHCARE CENTER',
                      'ALARIS HEALTH AT CEDAR GROVE', 'COMPLETE CARE AT HAMILTON, LLC')

#6th Wisconsin
# Sent by state
wisconsin_IDs <- c('52A407')
wisconsin_names <- c('TREMPEALEAU CTY HCC IMD')

#7th California 
# From previous reporting
california <- read.csv('INPUT_DATA/states_special_facilities/STPs_2022.csv')
california_IDs <- california$a0100b_cms_crtfctn_num

#8th Kentucky 
# Sent from state - these facilities are operated by the state for the purposes of providing care for people with SMI, though they don't necessarily receive any specific oversight
kentucky_names <- c('GLASGOW STATE NURSING FACILITY', 'WESTERN STATE NURSING FACILITY')
kentucky_IDs <- c('185363', '185228')

#9th Iowa
# Known in Iowa as ICF for mental illness, available here: https://dia-hfd.iowa.gov/Home/EntityPublicAdvancedSearch and confirmed with state via email 
iowa_IDs <- c('16F001', '165307', '16F002')
iowa_names <- c('DAVIS CENTER', 'PILLAR OF CEDAR VALLEY' , 'SOUTHEAST IOWA BEHAVIORAL HEALTH CARE CENTER')

#10th Maine
## "Maine has designated Med-Psych units in three nursing facilities. There are no separate licensing requirements for those beyond the regular nursing facility licenses. However, MaineCare (the state’s Medicaid program) provides a different reimbursement rate for those specialty services, which requires facilities to maintain separate accounting for these units."
# Names provided via email by state
maine_IDs <- c('205120','205098', '205166')
maine_names <- c("WATERVILLE CENTER FOR HEALTH AND REHAB","HAWTHORNE HOUSE", "GORHAM HOUSE")

#11th Minnesota
# Sent via state
minnesota_IDs <- c('24E116')
minnesota_names <- c('ANDREW RESIDENCE')

#12th Washington 
# Data sent by state 
wash <- read_excel('INPUT_DATA/states_special_facilities/WA_EBS-ECS-facilities.xlsx')
washington_IDs <- wash$`CMS ID`
washington_names <- wash$`Alternate name`

#13th New Hampshire - sent by state in email 
newhampshire_IDS <- c('30E059')
newhampshire_names <- c('Glencliff Home for the Elderly')

#14th Montana - found online, no other facilities noted by state
montana_IDs <- c('27A052')
montana_names <- c('Montana Mental Health Nursing Home')

# 15th South Carolina - found on mental health site 
south_carolina_IDs <- c('425360')
south_carolina_names <- c('C M Tucker Jr Nursing Care Center Roddey Pavilion')

# 16th Oregon - list sent via email 
oregon <- read_excel('INPUT_DATA/states_special_facilities/Oregon_ECU_ICC Data Request 10.31.24.xlsx')

#select just nursing facilities, not assisted living/RCF
oregon_NFs <- oregon %>%
  filter(`Facility Program Type` == 'NF')

oregon_IDs <- oregon_NFs$`Service Provider #`
oregon_names <- oregon_NFs$`Person/Service Provider Name`

#17th Mississippi - facilities sent via email; looked up IDs

mississippi_IDs <- c('25A418','25A123', '25A197', '25A402','25A404')
mississippi_names <- c('James T. Champion', 
'Reginald P. White Nursing Facility',
'Jnh-Jaquith Inn',
'Jnh-Jefferson Inn',
'Jnh-Madison Inn')

#18 Virginia - I found these independently; never received a response from the state . they are operated as behavioral facilities by the state's mental health dept
virginia_IDs <- c('49E131', '495113')
virginia_names <- c('SW VA M H Inst Geri Trt Ctr', 
                    'Hiram W Davis Medical Ctr')

#19 Wyoming - sent via email, "in our state, the Wyoming Department of Health operates safety net facilities that serve such patients."

wyoming_IDs <- c('535021', '535058')
wyoming_names <- c('Wyoming Retirement Center', 'Mountain View Skilled Nursing Community at Wlrc')

#20 North Dakota
north_dakota_IDs <- c('355063', '355048', '355077')
north_dakota_names <- c('St Lukes Home', 'Prince of Peace Care Center', 'Smp Health - St Raphael')

#create list of IDs
remove_cms <- c(colorado_IDs, additional_CO_IDs, idaho_IDs, kansas_IDs, 
                additional_KS_IDs, nevada_IDs, new_jersey_IDs, wisconsin_IDs, 
                california_IDs, kentucky_IDs, maine_IDs, minnesota_IDs, washington_IDs, 
                iowa_IDs, newhampshire_IDS, montana_IDs,
                south_carolina_IDs, oregon_IDs, mississippi_IDs, 
                virginia_IDs, wyoming_IDs, north_dakota_IDs)
#remove_name <- c('')

write.csv(remove_cms, 'DATA/facilities_to_remove.csv')
