#####################################################
# Title: 03_case_control_matching                   #
# Author: Izzie McLachlan                           #      
# Date: 02/04/25                                    #
# Description: reads in script to set-up cohort;    #
# case-control matching                             #
# Updates:                                          #
#####################################################

## 00 Set-up ##

## Read in the script to set-up the cohort
source("/conf/Resp_Surv_Data_Platform/Analysis/RSV/VE_Babies/code/01_input_data.R")

## 01 Case-control matching ##
n_controls <- 10

cohort <- cohort %>% mutate(isoweek_date_end_pregnancy = ISOweek(date_end_pregnancy),
                            gest_end_pregnancy2 = case_when(as.integer(gest_end_pregnancy) %in% c(35, 36) ~ 35,
                                                            as.integer(gest_end_pregnancy) %in% c(32, 33, 34) ~ 33,
                                                            as.integer(gest_end_pregnancy) < 32 ~ 30,
                                                            TRUE ~ as.integer(gest_end_pregnancy)))

matched <- ccwc(as.Date(entry_date), as.Date(exit_date), outcome2, as.Date(date_end_pregnancy), controls = n_controls, 
                data = cohort %>% filter(total_births_this_pregnancy == 1), # only matching singleton births
             include = list(resp_id_baby, resp_id_mother), 
             match = list(gest_end_pregnancy2, isoweek_date_end_pregnancy))


matched_cc <- cohort %>%
  left_join(matched, by = c("resp_id_baby", "resp_id_mother")) %>%
  filter(!is.na(Set))

# filter out sets where case is 6m+
set_0_3m <- matched_cc %>%
  filter(Fail == 1 & exit_age1 %in% c("0-3m")) %>%
  distinct(Set) %>% pull()

matched_cc <- matched_cc %>%
  filter(Set %in% c(set_0_3m))

## 

#count number of cases and controls
matched_cc %>%
  count(Fail)

# save matched case:control set
#saveRDS(matched,"/conf/Resp_Surv_Data_Platform/Analysis/RSV/VE_Babies/output/matched_case_control_resp.rds")
