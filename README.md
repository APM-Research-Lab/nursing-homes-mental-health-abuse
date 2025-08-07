# Nursing homes and mental health: Illness rates, abuse and staffing at facilities nationwide 
Code used in the analysis for an investigation on mental health and abuse in nursing homes

--------- 
This respository includes the code used for the investigation[ "‘We did not want to take this guy’: Abuse rates higher at nursing homes with more mental illness"](https://apmresearchlab.org/abuse-rates-higher-at-nursing-homes-with-many-mentally-ill-residents) APM Research Lab, with support from APM Reports, in an effort to be more transparent about our methodology. 

We created [an overview of our methods here](https://www.apmresearchlab.org/the-methods-behind-our-investigation-of-serious-mental-illness-and-abuse-citations-at-nursing-homes), and this repository is meant to provide further details, especially for those who may already be familiar with our main data source — the Minimum Data Set from the Centers for Medicare & Medicaid Services (CMS).

This investigation builds off a [project led by LAist](https://laist.com/news/specials/mental-illness-nursing-homes-california-what-we-found), for which we did [a similar analysis](https://github.com/APM-Research-Lab/nursing-homes-mental-health-California). 

Below is an overview of the scripts used for this analysis. 



Note: Underlying resident-level data from CMS is not provided in this repo at this time. 

1. Initial processing -> 1_snapshot_tree_style_2023.R
This step reads in the data received from CMS and creates a census version for 2023. The below diagram provides a conceptual framework of the methodology in this script. 

[image showing census method](https://images.squarespace-cdn.com/content/v1/5c9542c8840b163998cf4804/bb88b9c7-2e04-4fb3-8021-507442d1e1df/census_methodology_diagramV2%402x.png?format=2500w)


2. Initial processing -> 2_facilities_to_remove.R
This step creates a list of the facilities for which there is some special recognition or oversight for care of people with serious mental illness. These facilities will be excluded for most of our analyses. 

3. Initial processing ->  3_calculations_2023.R
This step adds a few calculated variables to the resident census dataset, and then summarizes the data at the facility level. 

4. Analysis -> 4_nationwide_overview.R
This script calculates the number and percent of people in nursing homes with serious mental illness. 

5. Analysis -> 5_facilities_over_half_SMI.R
This script calculates the number of facilities where at least half of the residents have an serious mental illness. 

6. Analysis -> 6_abuse_SMI.R
This script calculates the number of abuse citations per resident and per 10 percent interval of residents with serious mental illness at facilities. 

7. Analysis -> 7_abuse_SMI_staffing.R 
This script calculates how abuse citations vary with serious mental illness rates and staffing levels. 

8. Analysis -> 8_states_with_special_fac.R
This script calculates the number of abuse-related citations in states with some facilities that specialize in mental illness care, and compares the facilities with and without that oversight.  

9. Analysis -> 9_ownership_SMI.R
This script calculates the percent of homes with a high rate of mental illness that are for-profit, and compares that to the overall share of homes that are for-profit. 

10. Analysis -> 10_ownership_SMI_abuse.R
This script calculates the rates of abuse by both mental illness rate and ownership type (for-profit vs gov/nonprofit). 

11. Analysis -> 11_SMI_abuse_fines.R
This script calculates how often nursing homes are cited for abuse and levied a fine by the federal government. 

12. Initial processing - > 12_snapshot_tree_style_2013.R
This script pulls in data using the exact same code as script 1, but for 2013. 

13. Initial processing  -> 13_calculations_2013.R
This script processes the data using the exact same code as script 3, but for 2013.

14. Analysis - > 14_Illinois_2013_and_2023.R
This script compares the % of SMI residents in IL in 2013 and 2023. 

15. Initial processing -> 15_snapshot_tree_style_2023_for_PRIMARY_IL.R
This script uses the same census workflow as script 1, but with the primary medical diagnosis data for IL.

16. Initial processing -> 16_calculations_2023_for_PRIMARY_IL.R
This script defines mental illness from the diagnosis codes and then calculates and summarizes data at the facility level in IL, as in script 3. 

17. Analysis -> 17_Illinois_primary_diagonses.R
This script finds the number of facilities in Illinois where at least half of residents have a primary diagnosis of mental illness. It also calculates the number for North Aurora Care Center. 
