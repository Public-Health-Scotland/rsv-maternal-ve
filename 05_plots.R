#####################################################
# Title: 04_plots                                   #
# Author: Izzie McLachlan                           #      
# Date: 03/07/25                                    #
# Description: plots for paper                      #
# Updates:                                          #
#####################################################

## 00 Set-up ##

## Read in the script to set-up the cohort
source("/conf/Resp_Surv_Data_Platform/Analysis/RSV/VE_Babies/code/01_input_data.R")

## Plot theme
#standardized theme
phstheme <- theme(text = element_text(family = "Helvetica"),
                  axis.title = element_text(colour="black", family = "Helvetica"),
                  axis.text.x = element_text(vjust = 1,size=12),
                  axis.text.y = element_text(size=12),
                  axis.title.x = element_text(size=14),
                  axis.title.y = element_text(size=14),
                  panel.background = element_rect(colour = "white"),
                  axis.line.x = element_line(colour="black"),
                  axis.line.y = element_line(colour="black"),
                  plot.caption = element_text(hjust = 0, vjust =8),
                  legend.text = element_text(size=12.5, family="Helvetica"),
                  legend.title = element_text(size=14.5, family="Helvetica"),
                  legend.position = "bottom",
                  #legend.justification = c(0, 1),
                  legend.box = "horizontal",
                  legend.direction = "horizontal",
                  legend.key.size = unit(0.3, "cm"),
                  plot.margin = margin(t=2,r=0.5,b=0,l=0.5, "cm"),
                  legend.key = element_rect(fill = NA, colour = NA, size = 1)) 

# number and % vaccinated
cohort %>% 
  filter(total_births_this_pregnancy == 1) %>%
  count(protection)

cohort %>% 
  filter(total_births_this_pregnancy == 1) %>%
  count(protection, term)

# matched cases control 
matched_cc %>% count(Fail, protection)

# to calculate week at vaccination
#cohort
cohort_vacc <- cohort %>% 
  filter(total_births_this_pregnancy == 1) %>%
  filter(protection %in% c("partial", "vaccinated")) %>% 
  mutate(isoweek_vacc = as.numeric(as.Date(vacc_occurence_date)-as.Date(est_date_conception), units = "weeks")) %>%
  mutate(isoweek_vacc = floor(isoweek_vacc))

#summary averages - cohort
median(cohort_vacc$isoweek_vacc)
quantile(cohort_vacc$isoweek_vacc, 0.25)
quantile(cohort_vacc$isoweek_vacc, 0.75)

#matched cases and controls
matched_vacc <- matched_cc %>%
  filter(protection %in% c("partial", "vaccinated")) %>% 
  mutate(isoweek_vacc = as.numeric(as.Date(vacc_occurence_date)-as.Date(est_date_conception), units = "weeks")) %>%
  mutate(isoweek_vacc = floor(isoweek_vacc))

#summary averages - matched cases:controls
median(matched_vacc %>% filter(Fail == 1) %>% pull(isoweek_vacc))
quantile(matched_vacc %>% filter(Fail == 1) %>% pull(isoweek_vacc), 0.25)
quantile(matched_vacc %>% filter(Fail == 1) %>% pull(isoweek_vacc), 0.75)
wilcox.test(as.numeric(isoweek_vacc)~Fail, data = matched_vacc)

# mean age of cases
median(matched_cc %>% filter(Fail == 0) %>% mutate(plot_exit_age = as.numeric(as.Date(match_date)-as.Date(date_end_pregnancy))) %>% pull(plot_exit_age))
quantile(matched_cc %>% filter(Fail == 0) %>% mutate(plot_exit_age = as.numeric(as.Date(match_date)-as.Date(date_end_pregnancy))) %>% pull(plot_exit_age), 0.25)
quantile(matched_cc %>% filter(Fail == 0) %>% mutate(plot_exit_age = as.numeric(as.Date(match_date)-as.Date(date_end_pregnancy))) %>% pull(plot_exit_age), 0.75)
median(matched_cc %>% filter(Fail == 1) %>% mutate(plot_exit_age = as.numeric(exit_age)) %>% pull(plot_exit_age))
quantile(matched_cc %>% filter(Fail == 1) %>% pull(exit_age), 0.25)
quantile(matched_cc %>% filter(Fail == 1) %>% pull(exit_age), 0.75)

# mean gest age at birth cases
median(matched_cc %>% filter(Fail == 1) %>% pull(gest_end_pregnancy))
quantile(matched_cc %>% filter(Fail == 1) %>% pull(gest_end_pregnancy), 0.25)
quantile(matched_cc %>% filter(Fail == 1) %>% pull(gest_end_pregnancy), 0.75)
wilcox.test(as.numeric(gest_end_pregnancy)~Fail, data = matched_cc)

median(cohort %>% filter(total_births_this_pregnancy == 1, outcome != "RSV_adm") %>% pull(gest_end_pregnancy))
quantile(cohort %>% filter(total_births_this_pregnancy == 1, outcome != "RSV_adm") %>% pull(gest_end_pregnancy), 0.25)
quantile(cohort %>% filter(total_births_this_pregnancy == 1, outcome != "RSV_adm") %>% pull(gest_end_pregnancy), 0.75)

#birthweight
median(matched_cc %>% filter(Fail == 0) %>% pull(birthweight), na.rm=T)
quantile(matched_cc %>% filter(Fail == 0) %>% pull(birthweight), na.rm=T, 0.25)
quantile(matched_cc %>% filter(Fail == 0) %>% pull(birthweight), na.rm=T, 0.75)
wilcox.test(as.numeric(birthweight)~Fail, data = matched_cc)

median(cohort %>% filter(total_births_this_pregnancy == 1, outcome != "RSV_adm") %>% pull(birthweight), na.rm=T)
quantile(cohort %>% filter(total_births_this_pregnancy == 1, outcome != "RSV_adm") %>% pull(birthweight), na.rm=T, 0.25)
quantile(cohort %>% filter(total_births_this_pregnancy == 1, outcome != "RSV_adm") %>% pull(birthweight), na.rm=T, 0.75)

#maternal age
median(matched_cc %>% filter(Fail == 1) %>% pull(maternal_age_end_preg), na.rm=T)
quantile(matched_cc %>% filter(Fail == 1) %>% pull(maternal_age_end_preg), na.rm=T, 0.25)
quantile(matched_cc %>% filter(Fail == 1) %>% pull(maternal_age_end_preg), na.rm=T, 0.75)
wilcox.test(as.numeric(maternal_age_end_preg)~Fail, data = matched_cc)

median(cohort %>% filter(total_births_this_pregnancy == 1, outcome != "RSV_adm") %>% pull(maternal_age_end_preg), na.rm=T)
quantile(cohort %>% filter(total_births_this_pregnancy == 1, outcome != "RSV_adm") %>% pull(maternal_age_end_preg), na.rm=T, 0.25)
quantile(cohort %>% filter(total_births_this_pregnancy == 1, outcome != "RSV_adm") %>% pull(maternal_age_end_preg), na.rm=T, 0.75)

#time btw vacc and delivery
median(matched_cc %>% filter(Fail == 0) %>% pull(time_to_birth), na.rm=T)
quantile(matched_cc %>% filter(Fail == 0) %>% pull(time_to_birth), na.rm=T, 0.25)
quantile(matched_cc %>% filter(Fail == 0) %>% pull(time_to_birth), na.rm=T, 0.75)
wilcox.test(as.numeric(time_to_birth)~Fail, data = matched_cc)

median(cohort %>% filter(total_births_this_pregnancy == 1, outcome != "RSV_adm") %>% pull(time_to_birth), na.rm=T)
quantile(cohort %>% filter(total_births_this_pregnancy == 1, outcome != "RSV_adm") %>% pull(time_to_birth), na.rm=T, 0.25)
quantile(cohort %>% filter(total_births_this_pregnancy == 1, outcome != "RSV_adm") %>% pull(time_to_birth), na.rm=T, 0.75)

# cases in infants 0-3 months during study period
cohort_plot <- cohort %>% 
  filter(total_births_this_pregnancy == 1, exit_age1 %in% c("0-3m")) %>%
  filter(exit_date<dmy(31032025)) # so that we are showing complete weeks

# cumulative number vaccinated
cum_births <- cohort %>%
  filter(total_births_this_pregnancy == 1) %>%
  mutate(count_birth = 1,
         count_vacc = ifelse(protection %in% c("partial", "vaccinated"), 1, 0)) %>%
  arrange(isoweek_date_end_pregnancy) %>%
  mutate(cum_sum_birth = cumsum(count_birth),
         cum_sum_vacc = cumsum(count_vacc)) %>%
  select(isoweek_date_end_pregnancy, cum_sum_birth, cum_sum_vacc) %>%
  group_by(isoweek_date_end_pregnancy) %>%
  summarise(births = max(cum_sum_birth, na.rm = TRUE),
            vaccs = max(cum_sum_vacc, na.rm = TRUE)) %>%
  mutate(den = births,
                   num = vaccs,
                  prec_vacc = round(num/den*100,1)) %>%
           mutate(date = ISOweek2date(str_c(isoweek_date_end_pregnancy,"-1")))

cohort_plot <- cohort_plot %>%
  filter(outcome == "RSV_adm") %>%
  mutate(iso_week = ISOweek(exit_date)) %>%
  count(iso_week) %>%
  mutate(date = ISOweek2date(str_c(iso_week,"-1"))) 


# figure 1
tiff("/conf/Resp_Surv_Data_Platform/Analysis/RSV/VE_Babies/output/figure1.tiff", units="in", width=7, height=5, res=300)
ggplot() +
  geom_bar(data=cohort_plot, aes(x = date, y = n, fill = "hosp_adms"), stat = "identity") +
  geom_line(data=cum_births, aes(x = date, y = prec_vacc/2, colour = "vacc")) +
  theme_bw() +
  phstheme +
  scale_x_date(date_labels = "%b %Y", breaks = "2 months") +
  labs(x = "Date", y = "Number of RSV-related\nadmissions per week", fill = "", colour = "") +
  scale_colour_manual(values = c("blue"),
                      breaks = c("vacc"),
                      labels = c("Maternal RSV vaccination")) +
  scale_fill_manual(values = c("grey"),
                      breaks = c("hosp_adms"),
                      labels = c("RSV-related hospital admissions")) +
  scale_y_continuous(sec.axis = sec_axis(~ .*2, name = "Cumulative percentage of infants whose\nmothers received the RSV vaccine (%)"))
dev.off()

# summary tables 1 and 2
cohort %>%
  filter(total_births_this_pregnancy == 1) %>%
  mutate(gest_group = case_when(gest_end_pregnancy < 37 ~ "<37",
                                gest_end_pregnancy >= 37 ~ ">=37",
                                TRUE ~ "other")) %>%
  tbl_summary(by=outcome2, 
              include = c(protection, baby_sex, maternal_smoking, gest_group, previous_delivery, maternal_simd_end_preg, maternal_age_gp, maternal_ethnicity),
              label = list(protection = "Vaccination",
                           baby_sex = "Baby sex",
                           maternal_smoking = "Maternal smoking",
                           gest_group = "Gestational age a birth",
                           previous_delivery = "Parity",
                           maternal_simd_end_preg = "SIMD", 
                           maternal_age_gp = "Maternal age",
                           maternal_ethnicity = "Maternal ethnicity")) %>%
  add_p()

matched_cc %>%
tbl_summary(by=Fail, 
            include = c(protection, baby_sex, maternal_smoking, gest_group, previous_delivery, maternal_simd_end_preg, maternal_age_gp, maternal_ethnicity),
            label = list(protection = "Vaccination",
                         baby_sex = "Baby sex",
                         maternal_smoking = "Maternal smoking",
                         gest_group = "Gestational age a birth",
                         previous_delivery = "Parity",
                         maternal_simd_end_preg = "SIMD", 
                         maternal_age_gp = "Maternal age",
                         maternal_ethnicity = "Maternal ethnicity")) %>%
  add_p()

table(matched_cc$Fail, matched_cc$maternal_simd_end_preg) %>% chisq.test()

# forest plot
library(forestplot)
#Extract the coefficients, confidence intervals, and p-values for the forest plot.
# Extract coefficients and confidence intervals
# Combine results into a table
results_main <- data.frame(
  Variable = rownames(exp(summary(model2)$coefficients)),
  Estimate = (1-exp(summary(model2)$coefficients)[, "coef"])*100,
  LowerCI = (1-exp(confint(model2))[, 2])*100,
  UpperCI = (1-exp(confint(model2))[, 1])*100,
  pValue = (summary(model2)$coefficients)[, "Pr(>|z|)"]
)

results_main <- results_main %>% 
  filter(Variable %in% c("protectionvaccinated", "protectionpartial")) %>%
  mutate(Variable = case_when(Variable == "protectionvaccinated" ~ "Vaccinated",
                              Variable == "protectionpartial" ~ "Sub-optimal immunisation")) %>%
  mutate(pValue = ifelse(pValue < 0.001, "<0.001", format.pval(pValue, digits = 2)))

# gest age at birth
results_gest_age <- data.frame(
  Variable = rownames(exp(summary(model4_1)$coefficients)),
  Estimate = (1-exp(summary(model4_1)$coefficients)[, "coef"])*100,
  LowerCI = (1-exp(confint(model4_1))[, 2])*100,
  UpperCI = (1-exp(confint(model4_1))[, 1])*100,
  pValue = (summary(model4_1)$coefficients)[, "Pr(>|z|)"]
)

results_gest_age <- results_gest_age %>% 
  filter(Variable %in% c("gest_group<37:protectionvaccinated", "gest_group>=37:protectionvaccinated")) %>%
  mutate(Variable = case_when(Variable == "gest_group<37:protectionvaccinated" ~ "    <37 weeks gestation",
                              Variable == "gest_group>=37:protectionvaccinated" ~ "    >=37 weeks gestation"))  %>%
  mutate(pValue = ifelse(pValue < 0.001, "<0.001", format.pval(pValue, digits = 2)))

num_vacc <- matched_cc %>%
  count(Fail, protection)
num_vacc_gest <- matched_cc %>%
  filter(protection == "vaccinated") %>%
  count(Fail, gest_group)

# Prepare data for forestplot
tabletext <- cbind(
  c("", "Variable", "", "Unvaccinated", results_main$Variable[2], results_gest_age$Variable, results_main$Variable[1]),
  c("", "Cases", "", num_vacc %>% filter(Fail==1, protection == "unvaccinated") %>% pull(n),
    num_vacc %>% filter(Fail==1, protection == "vaccinated") %>% pull(n), 
    num_vacc_gest %>% filter(Fail==1, gest_group == "<37") %>% pull(n), 
    num_vacc_gest %>% filter(Fail==1, gest_group == ">=37") %>% pull(n),
    num_vacc %>% filter(Fail==1, protection == "partial") %>% pull(n)),
  c("", "Controls", "", num_vacc %>% filter(Fail==0, protection == "unvaccinated") %>% pull(n),
    num_vacc %>% filter(Fail==0, protection == "vaccinated") %>% pull(n), 
    num_vacc_gest %>% filter(Fail==0, gest_group == "<37") %>% pull(n), 
    num_vacc_gest %>% filter(Fail==0, gest_group == ">=37") %>% pull(n),
    num_vacc %>% filter(Fail==0, protection == "partial") %>% pull(n)),
  c("Vaccine", "Effectiveness", "% (95% CI)", "",
    paste0(round(results_main$Estimate[2], 1), " (", 
           round(results_main$LowerCI[2], 1), ", ", 
           round(results_main$UpperCI[2], 1), ")"),
    paste0(round(results_gest_age$Estimate, 1), " (", 
           round(results_gest_age$LowerCI, 1), ", ", 
           round(results_gest_age$UpperCI, 1), ")"),
    paste0(round(results_main$Estimate[1], 1), " (", 
           round(results_main$LowerCI[1], 1), ", ", 
           round(results_main$UpperCI[1], 1), ")")),
  c("", "p-value", "", "", results_main$pValue[2],results_gest_age$pValue, results_main$pValue[1])
)

# Forest plot
tiff("/conf/Resp_Surv_Data_Platform/Analysis/RSV/VE_Babies/output/figure2.tiff", units="in", width=10, height=3, res=300)
forestplot(
  labeltext = tabletext,
  graph.pos = 4,graphwidth=unit(5,"cm"), 
  boxsize = 0.25,
  mean = c(NA, NA, NA, NA, results_main$Estimate[2], results_gest_age$Estimate, results_main$Estimate[1]),
  lower = c(NA, NA, NA, NA, results_main$LowerCI[2], results_gest_age$LowerCI, results_main$LowerCI[1]),
  upper = c(NA, NA, NA, NA, results_main$UpperCI[2], results_gest_age$UpperCI, results_main$UpperCI[1]),
  zero = 0,
  col = fpColors(box = "royalblue", lines = "darkblue", summary = "royalblue"), 
  hrzl_lines = list("4" = gpar(lwd = 1),
                    "6" = gpar(lwd = 0.5),
                    "8" = gpar(lwd = 0.5)),  # Add a horizontal line
  # Bold titles
  is.summary = c(TRUE, TRUE, TRUE, 
                 rep(FALSE, 5), 
                 TRUE, rep(FALSE, 3), 
                 TRUE, rep(FALSE, 4)),
  lineheight = unit(0.5, "cm"),
  line.margin = 0.1,
  txt_gp = fpTxtGp(label = gpar(cex = 1),
                   summary = gpar(cex = 1),
                   xlab = gpar(cex = 1),
                   ticks = gpar(cex = 1)), # Size of x axis ticks
  xticks = c(0, 25, 50, 75, 100), # X axis breaks
  xlab = "Vaccine Effectiveness (%)",
  title = ""
)
dev.off()

