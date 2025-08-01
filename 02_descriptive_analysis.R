#####################################################
# Title: 02_descriptive_analysis                    #
# Author: Izzie McLachlan                           #      
# Date: 02/04/25                                    #
# Description: exploratory and descriptive analysis #
# of the cohort                                     #
# Updates:                                          #
#####################################################

## 00 Set-up ##

## Read in the script to set-up the cohort
source("/conf/Resp_Surv_Data_Platform/Analysis/RSV/VE_Babies/code/01_input_data.R")

## 01 Plots ##

## Trend of live births
ggplot() +
  geom_bar(data = cohort, aes(x = date_end_pregnancy)) +
  theme_bw() +
  labs(x = "Date", y = "Number of live births")

## Distribution of maternal age
ggplot() +
  geom_histogram(data = cohort, aes(x = maternal_age_end_preg), binwidth=1) +
  theme_bw() +
  labs(x = "Maternal age (years)", y = "Count")

## Maternal SIMD
ggplot() +
  geom_bar(data = cohort, aes(x = maternal_simd_end_preg)) +
  theme_bw() +
  labs(x = "Maternal age (years)", y = "Count")

## Vaccination
cohort %>%
  count(month(date_end_pregnancy), protection) %>%
  ggplot() +
  geom_bar(aes(x = date_end_pregnancy, y = n, fill = protection)) +
  theme_bw() +
  labs(x = "Maternal age (years)", y = "Count")
# Time from vaccination to birth
ggplot() +
  geom_histogram(data=cohort %>% filter(!is.na(vacc_product_name)), mapping=aes(x = time_to_birth), stat = "count") +
  geom_vline(aes(xintercept = 14)) +
  theme_bw() +
  labs(x = "Days from vaccination to birth")

## 02 Proportion vaccinated ##

# Baby sex
round(prop.table(table(cohort$protection, cohort$baby_sex), margin = 2), 3)
# tidycmprsk::cuminc(Surv(time, as.factor(protection)) ~ baby_sex, cohort) %>%
#   ggcuminc(outcome = "vaccinated") +
#   scale_ggsurvfit() +
#   labs(y = "Cumulative Incidence\n(vaccination)")

# Previous deliveries
round(prop.table(table(cohort$protection, cohort$previous_delivery), margin = 2), 3)
# tidycmprsk::cuminc(Surv(time, as.factor(protection)) ~ previous_delivery, cohort) %>%
#   ggcuminc(outcome = "vaccinated") +
#   scale_ggsurvfit() +
#   labs(y = "Cumulative Incidence\n(vaccination)")

# Births this pregnancy
round(prop.table(table(cohort$protection, cohort$total_births_this_pregnancy), margin = 2), 3)
# tidycmprsk::cuminc(Surv(time, as.factor(protection)) ~ total_births_this_pregnancy, cohort) %>%
#   ggcuminc(outcome = "vaccinated") +
#   scale_ggsurvfit() +
#   labs(y = "Cumulative Incidence\n(vaccination)")

# Term
round(prop.table(table(cohort$protection, cohort$term), margin = 2), 3)
# tidycmprsk::cuminc(Surv(time, as.factor(protection)) ~ term, cohort) %>%
#   ggcuminc(outcome = "vaccinated") +
#   scale_ggsurvfit() +
#   labs(y = "Cumulative Incidence\n(vaccination)")

# Maternal SIMD
round(prop.table(table(cohort$protection, cohort$maternal_simd_end_preg), margin = 2), 3)
# tidycmprsk::cuminc(Surv(time, as.factor(protection)) ~ maternal_simd_end_preg, cohort) %>%
#   ggcuminc(outcome = "vaccinated") +
#   scale_ggsurvfit() +
#   labs(y = "Cumulative Incidence\n(vaccination)")

# Birth weight
round(prop.table(table(cohort$protection, cohort$birthweight_group), margin = 2), 3)
# tidycmprsk::cuminc(Surv(time, as.factor(protection)) ~ birthweight_group, cohort) %>%
#   ggcuminc(outcome = "vaccinated") +
#   scale_ggsurvfit() +
#   labs(y = "Cumulative Incidence\n(vaccination)")

# Maternal smoking
round(prop.table(table(cohort$protection, cohort$maternal_smoking), margin = 2), 3)
# tidycmprsk::cuminc(Surv(time, as.factor(protection)) ~ maternal_smoking, cohort) %>%
#   ggcuminc(outcome = "vaccinated") +
#   scale_ggsurvfit() +
#   labs(y = "Cumulative Incidence\n(vaccination)")

# Maternal age
round(prop.table(table(cohort$protection, cohort$maternal_age_gp), margin = 2), 3)
# tidycmprsk::cuminc(Surv(time, as.factor(protection)) ~ maternal_age_gp, cohort) %>%
#   ggcuminc(outcome = "vaccinated") +
#   scale_ggsurvfit() +
#   labs(y = "Cumulative Incidence\n(vaccination)")

## 03 Proportion/cumulative incidence RSV admissions ##

# Baby sex
# Proportion
round(prop.table(table(cohort$outcome, cohort$baby_sex), margin = 2), 3)
# Cumulative incidence
tidycmprsk::cuminc(Surv(time, as.factor(outcome)) ~ baby_sex, cohort) %>%
  ggcuminc(outcome = "RSV_adm") +
  scale_ggsurvfit() +
  labs(x = "Days to RSV admissions")

# Previous deliveries
# Proportion
round(prop.table(table(cohort$outcome, cohort$previous_delivery), margin = 2), 3)
# Cumulative incidence
tidycmprsk::cuminc(Surv(time, as.factor(outcome)) ~ previous_delivery, cohort) %>%
  ggcuminc(outcome = "RSV_adm") +
  scale_ggsurvfit() +
  labs(x = "Days to RSV admissions")

# Births this pregnancy
# Proportion
round(prop.table(table(cohort$outcome, cohort$total_births_this_pregnancy), margin = 2), 3)
# Cumulative incidence
tidycmprsk::cuminc(Surv(time, as.factor(outcome)) ~ total_births_this_pregnancy, cohort) %>%
  ggcuminc(outcome = "RSV_adm") +
  scale_ggsurvfit() +
  labs(x = "Days to RSV admissions")

# Term
# Proportion
round(prop.table(table(cohort$outcome, cohort$term), margin = 2), 3)
# Cumulative incidence
tidycmprsk::cuminc(Surv(time, as.factor(outcome)) ~ term, cohort) %>%
  ggcuminc(outcome = "RSV_adm") +
  scale_ggsurvfit() +
  labs(x = "Days to RSV admissions")

# Maternal SIMD
# Proportion
round(prop.table(table(cohort$outcome, cohort$maternal_simd_end_preg), margin = 2), 3)
# Cumulative incidence
tidycmprsk::cuminc(Surv(time, as.factor(outcome)) ~ maternal_simd_end_preg, cohort) %>%
ggcuminc(outcome = "RSV_adm") +
  scale_ggsurvfit() +
  labs(x = "Days to RSV admissions") 

# Birth weight
# Proportion
round(prop.table(table(cohort$outcome, cohort$birthweight_group), margin = 2), 3)
# Cumulative incidence
tidycmprsk::cuminc(Surv(time, as.factor(outcome)) ~ birthweight_group, cohort) %>%
  ggcuminc(outcome = "RSV_adm") +
  scale_ggsurvfit() +
  labs(x = "Days to RSV admissions")

# Maternal smoking
# Proportion
round(prop.table(table(cohort$outcome, cohort$maternal_smoking), margin = 2), 3)
# Cumulative incidence
tidycmprsk::cuminc(Surv(time, as.factor(outcome)) ~ maternal_smoking, cohort) %>%
  ggcuminc(outcome = "RSV_adm") +
  scale_ggsurvfit() +
  labs(x = "Days to RSV admissions")

# Maternal age
# Proportion
round(prop.table(table(cohort$outcome, cohort$maternal_age_gp), margin = 2), 3)
# Cumulative incidence
tidycmprsk::cuminc(Surv(time, as.factor(outcome)) ~ maternal_age_gp, cohort) %>%
  ggcuminc(outcome = "RSV_adm") +
  scale_ggsurvfit() +
  labs(x = "Days to RSV admissions")

