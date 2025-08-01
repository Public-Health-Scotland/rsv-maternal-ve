#####################################################
# Title: 01_input_data                              #
# Author: Izzie McLachlan                           #      
# Date: 02/04/25                                    #
# Description: reads in data and sets-up the cohort #
# Updates:                                          #
#####################################################

## 00 Set-up ##
#loadNamespace("bit64")
## Packages 

library(tidyverse)
library(stringi)
library(ggplot2)
library(Epi)
library(survival)
library(tidycmprsk)
library(ggsurvfit)
library(ISOweek)
library(gtsummary)

## Parameters and file 

'%!in%' <- function(x,y)!('%in%'(x,y))
output_list <- list()  # to save study parameters and output

#start and end dates for ve hospitalisations
start_date <- as_date("2024-08-12") # date vaccine programme started
end_date <- as_date("2025-03-31")
#start and end dates for vaccine uptake
#start_date <- as_date("2024-08-05") -90 # date of first vaccine administered
#end_date <- as_date("2025-01-31") + 90
output_list$start_date <- start_date
output_list$end_date <- end_date

data_folder <- "/conf/Resp_Surv_Data_Platform/Data/"

lrti <- c("J100", "J110", "J12", "J13", "J14", "J15", "J18", "J20", "J21", "J22", "J440", "J90")

## 01 Data ##

## Read in data
slipbd_maternity <- readRDS(paste0(data_folder,"slipbd_maternity.rds")) %>%
  janitor::clean_names()
smr01 <- readRDS(paste0(data_folder,"smr01_admits.rds")) %>%
  janitor::clean_names()
ecoss_rsv_tests <- readRDS(paste0(data_folder,"ecoss/ecoss_rsv_tests.rds")) %>%
  janitor::clean_names()
vacc_rsv <- readRDS(paste0(data_folder,"vaccinations/vacc_rsv.rds")) %>%
  janitor::clean_names()
nrs_deaths <- readRDS(paste0(data_folder,"nrs_deaths.rds")) %>%
  janitor::clean_names()
gp_demographics_latest <- readRDS(paste0(data_folder,"gp data/gp_demographics_latest.rds")) %>%
  janitor::clean_names()

## Filter maternity data for live births since introduction of vaccine to end date
maternity <- slipbd_maternity %>%
  filter(!is.na(date_end_pregnancy) & date_end_pregnancy >= start_date & date_end_pregnancy <= end_date) %>%
  filter(fetus_outcome1=="Live birth")

## Maternal vaccination
maternal_vaccination <- vacc_rsv %>% filter(resp_id %in% maternity$resp_id_mother) 

## Baby deaths
nrs_deaths <- nrs_deaths %>% filter(resp_id %in% maternity$resp_id_baby)
babies_deaths <- maternity %>% left_join(nrs_deaths, by = c("resp_id_baby"="resp_id")) %>%
  filter(infant_death == 1 | !is.na(nrs_date_death)) %>% select(resp_id_mother, resp_id_baby, infant_death, date_infant_death, nrs_date_death) #%>%

## Baby GP data (use to determine if babies transferred out during the study period)
babies_gp <- gp_demographics_latest %>% filter(resp_id %in% maternity$resp_id_baby)

## Baby RSV admissions
hosp_babies <- smr01 %>% filter(admission_date >= start_date) %>% filter(resp_id %in% maternity$resp_id_baby) # any hospital admission
#positive RSV test within 14 days before to 2 days after admission, emergency admissions with a resp main diagnosis code, first RSV admission per individual
babies_rsv_hosp <- hosp_babies %>% 
  left_join(ecoss_rsv_tests, by="resp_id", multiple="all", relationship="many-to-many") %>% 
  filter(result=="Positive") %>%
  mutate(date_diff = as.Date(admission_date)-as.Date(specimen_date)) %>%
  filter(date_diff >= -2, date_diff <= 14) %>%
  #create a flag for any lrti diagnosis
  mutate(lrti_flag = if_any(
    .cols = c(main_diag_admit, starts_with("other_diag")),
    .fns = ~ str_detect(.x, paste(lrti, collapse = "|"))
  )) %>%
  replace_na(list(lrti_flag = FALSE)) %>%
  #create a flag for lrti main diagnosis
  mutate(lrti_flag_main = if_any(
  .cols = c(main_diag_admit),
  .fns = ~ str_detect(.x, paste(lrti, collapse = "|"))
  )) %>%
  replace_na(list(lrti_flag_main = FALSE)) 
babies_rsv_hosp <- babies_rsv_hosp %>%
  #filter(emergency_admission_flag == 1, resp_main_diag_admit == 1) %>% # main diagnosis is resp code J00-99 + covid
  #filter(emergency_admission_flag == 1 & (resp_main_diag_admit == 1 | resp_other_cond_admit == 1)) %>% ## uncomment for any respiratory diagnosis # any diagnosis code is resp J00-99 + covid
  #filter(emergency_admission_flag == 1 & lrti_flag_main == "TRUE") %>% # filter for lrti main diagnosis
  filter(emergency_admission_flag == 1 & lrti_flag == "TRUE") %>% ## uncomment for any LRTI diagnosis # filter for lrti any diagnosis
  arrange(resp_id, admission_date) %>% filter(!duplicated(resp_id))

## 02 Cohort ##

## Join data to create cohort
cohort <- maternity %>% 
  left_join(nrs_deaths, by = c("resp_id_baby"="resp_id")) %>%
  left_join(babies_gp, by = c("resp_id_baby"="resp_id")) %>%
  left_join(babies_rsv_hosp %>% select(-c(vacc_occurence_date, vacc_product_name, specimen_date)), by = c("resp_id_baby" = "resp_id")) %>% 
  left_join(maternal_vaccination %>% select(-c(date_end_pregnancy)), by = c("resp_id_mother" = "resp_id")) %>% 
  left_join(ecoss_rsv_tests %>% filter(result == "Positive") %>% 
              group_by(resp_id) %>% arrange(specimen_date) %>% filter(row_number()==1) %>% select(resp_id, specimen_date), by = c("resp_id_baby"="resp_id")) %>%
  mutate(entry_date = as.Date(date_end_pregnancy),
         exit_date_temp = pmin(as.Date(admission_date), as.Date(nrs_date_death), as.Date(date_infant_death), as.Date(date_transfer_out), as.Date(end_date), na.rm = T)) %>% 
  mutate(outcome = case_when(exit_date_temp == admission_date ~ "RSV_adm",
                             exit_date_temp == nrs_date_death ~ "death",
                             exit_date_temp == date_infant_death ~ "death",
                             exit_date_temp == date_transfer_out ~ "transferred_out",
                             TRUE ~ "end")) %>%
  mutate(exit_date = case_when((outcome != "RSV_adm" & pmin(exit_date_temp, specimen_date, na.rm = T) != exit_date_temp) ~ specimen_date,
                               TRUE ~ exit_date_temp),
         outcome = case_when(exit_date != exit_date_temp ~ "RSV positive only",
                             TRUE ~ outcome)) %>%
  mutate(term = case_when(as.integer(gest_end_pregnancy) >=37 ~ "term >=37w",
                          between(as.integer(gest_end_pregnancy), 32, 36) ~ "preterm 32-36w",
                          as.integer(gest_end_pregnancy) < 32 ~ "very preterm <32w",
                          TRUE ~ "other")) %>%
  #vaccination
  mutate(time_to_birth = as.Date(date_end_pregnancy)-as.Date(vacc_occurence_date)) %>%
  mutate(protection = case_when(is.na(vacc_product_name) ~ "unvaccinated",
                                time_to_birth < 0 ~ "unvaccinated",
                                #time_to_birth < 0 ~ "after birth",
                                between(as.numeric(time_to_birth), 0, 14) ~ "partial", 
                                time_to_birth > 14 ~ "vaccinated", 
                                TRUE ~ "other"),
         vaccinated = ifelse((!is.na(vacc_product_name)&time_to_birth >= 0), 1, 0),
         time = as.Date(exit_date)-as.Date(entry_date)) %>%
  mutate(birthweight_group = case_when(birthweight < 1500 ~ "very low",
                                       between(birthweight, 1500, 2499) ~ "low",
                                       birthweight >= 2500 ~ "normal"),
         previous_delivery = case_when(n_prev_deliveries == 0 ~ "0",
                                       n_prev_deliveries == 1 ~ "1",
                                       n_prev_deliveries >1 ~ "2+"),
         maternal_age_gp = case_when(as.integer(maternal_age_end_preg) < 25 ~ "<25",
                                     between(as.integer(maternal_age_end_preg), 25, 29) ~ "25-29",
                                     between(as.integer(maternal_age_end_preg), 30, 34) ~ "30-34",
                                     as.integer(maternal_age_end_preg) >= 35 ~ "35+")) %>%
  mutate(exit_age = as.Date(exit_date)-as.Date(date_end_pregnancy)) %>%
  mutate(exit_age1 = case_when(exit_age<90 ~ "0-3m",
                               between(as.numeric(exit_age), 91, 180) ~ "4-6m",
                               TRUE ~ "6m+")) %>%
  mutate(outcome2 = ifelse((outcome == "RSV_adm" & exit_age1 == "0-3m"), 1, 0)) 


