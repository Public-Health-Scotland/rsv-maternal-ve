#####################################################
# Title: 04_model                                   #
# Author: Izzie McLachlan                           #      
# Date: 02/04/25                                    #
# Description: reads in script to set-up cohort and #
# case-control matching; logistic regression model  #
# Updates:                                          #
#####################################################

## 00 Set-up ##

## Read in the script to set-up the cohort
source("/conf/Resp_Surv_Data_Platform/Analysis/RSV/VE_Babies/code/01_input_data.R")

## Read in the script to match cases and controls
source("/conf/Resp_Surv_Data_Platform/Analysis/RSV/VE_Babies/code/03_case_control_matching.R")

## 01 Model vaccination only ##
matched_cc$protection <- factor(matched_cc$protection, levels = c("unvaccinated", "partial", "vaccinated")) #re-level so unvaccinated is the reference group
model1 <- clogit(outcome2 ~ protection+strata(Set), matched_cc)
gtsummary::tbl_regression(model1, exponentiate = TRUE, estimate_fun = purrr::partial(style_ratio, digits = 3)) 

## 02 Model vaccination + other co-variates ##
model2 <- clogit(outcome2 ~ protection+maternal_simd_end_preg+baby_sex+maternal_age_gp+maternal_smoking+birthweight_group+previous_delivery+maternal_ethnicity+strata(Set), matched_cc)# %>% filter(exit_age1=="0-3m")
gtsummary::tbl_regression(model2, exponentiate = TRUE, estimate_fun = purrr::partial(style_ratio, digits = 3))

## 04 Model vaccination + covariates and interaction term ##
matched_cc$protection <- factor(matched_cc$protection, levels = c("unvaccinated", "partial", "vaccinated")) #re-level so unvaccinated is the reference group
matched_cc_cases <- matched_cc %>% # get the age of cases at RSV admission
  select(-exit_age, -exit_age1) %>%
  filter(Fail == 1) %>%
  mutate(exit_age = as.Date(exit_date)-as.Date(date_end_pregnancy)) %>%
  mutate(exit_age1 = case_when(exit_age<90 ~ "0-3m",
                               between(as.numeric(exit_age), 91, 180) ~ "4-6m",
                               TRUE ~ "6m+")) %>%
  group_by(Set) %>%
  mutate(match_date = min(as.Date(exit_date))) %>%
  ungroup() %>%
  select(Set, exit_age, exit_age1, match_date) %>%
  distinct()
matched_cc <- matched_cc %>%
  select(-exit_age, -exit_age1) %>%
  mutate(gest_group = case_when(gest_end_pregnancy < 37 ~ "<37",
                                gest_end_pregnancy >= 37 ~ ">=37",
                                TRUE ~ "other")) %>%
  left_join(matched_cc_cases, by="Set") # apply age of cases at admission to entire case:control set
matched_cc <- matched_cc %>%
  mutate(protection = as.factor(protection),
         maternal_simd_end_preg = as.factor(maternal_simd_end_preg),
         birthweight_group = as.factor(birthweight_group))

#vaccination:gestational age at birth interaction
model4 <- clogit(outcome2 ~ protection:gest_group+gest_group+strata(Set), matched_cc)
gtsummary::tbl_regression(model4, exponentiate = TRUE, estimate_fun = purrr::partial(style_ratio, digits = 3))

#vaccination:gestational age at birth interaction
model4_1 <- clogit(outcome2 ~ protection:gest_group+gest_group+maternal_simd_end_preg+baby_sex+maternal_age_gp+maternal_smoking+birthweight_group+previous_delivery+maternal_ethnicity+strata(Set), matched_cc)
gtsummary::tbl_regression(model4_1, exponentiate = TRUE, estimate_fun = purrr::partial(style_ratio, digits = 3))


