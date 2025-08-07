# Vaccine effectiveness of maternal RSV vaccine against RSV-related hospitalisation in infants


Estimating the vaccine effectiveness of the maternal RSV vaccine against RSV-related hospitalisation in infants aged <=90 days, in Scotland, 12 August 2024 to 31 March 2025.

**Content**


- **01_input_data.R** - creates cohort of live births between 12 August 2024 and 31 March 2025 and links to maternal RSV vaccination records, and infant deaths, RSV related hospital admissions 
- **02_descriptive_analysis.R** - initial exploration and descriptive analysis of the cohort (RSV-related admissions in infants and maternal RSV vaccination)
- **03_case_control_matching.R** - match cases to controls (1:10) for nested case-control design
- **04_model.R** - conditional logistic regression modelling unadjusted and adjusted models and model with interaction term (gestational age at birth and vaccination status)
- **05_plots.R** - summary tables and figures
