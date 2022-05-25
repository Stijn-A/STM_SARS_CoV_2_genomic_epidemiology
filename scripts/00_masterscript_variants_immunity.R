# Author: Stijn P. Andeweg (email: stijn.andeweg@rivm.nl)
# Title: Elevated risk of infection with SARS-CoV-2 Beta, Gamma, and Delta variant compared to Alpha variant in vaccinated individuals

setwd("/YOURPATH/STM_SARS_CoV_2_genomic_epidemiology/")

### initialize PATH data
source(file = "scripts/01_init_PATHS.R")

## packages
lapply(c("tidyverse", "rlang", "lubridate", "readxl", "writexl", "broom.mixed", "ISOweek", 
         "splines", "grid", "cowplot", "VGAM"), require, character.only = TRUE)

## options
options(dplyr.summarise.inform = FALSE) # Silence de grouping message
options(scipen = 999) # Do not use exponential notations 

### import
source(file = "scripts/01_import_VI.R") 
### data preparation
source(file = "scripts/02_data_cleaning_VI.R") 

### Tables and Figures
## Figure 1, S1, and S2
source(file = "scripts/03_figure_immunestatus.R") 
## Figure 2 and S3 -- Generalized linear model multinomial logistic regression
source(file = "scripts/04_GLM_MLR_immunestatus.R") 

## Table 2
source(file = "scripts/04_GLM_MLR_vaccin.R") 

source(file = "scripts/04_GLM_MLR_variant_waning.R") 

## Table 1
source(file = "scripts/05_numbers_in_text.R") 



### Saving
PATH_figures <- "figures/" 

ggsave(file = str_c('F1_variants_immune_status_', format(now(), format = "%Y%m%d_%H%M"), ".pdf"), plot = F1_immune_status,
      width = 20, height = 10)

ggsave(file = str_c('F2_GLM_LR_variant_immune_status_adjusted_', format(now(), format = "%Y%m%d_%H%M"), ".pdf"), plot = F2_variant_immune_adjusted,
       width = 12, height = 9)

ggsave(file = str_c('S1_osiris_immune_status_', format(now(), format = "%Y%m%d_%H%M"), ".png"), plot = S1_osiris_immune_status,
       width = 14, height = 7, dpi = 600)

ggsave(file = str_c('S2_variants_immune_status_', format(now(), format = "%Y%m%d_%H%M"), ".png"), plot = S2_immune_status,
       width = 15, height = 10, dpi = 600)

ggsave(file = str_c('S3_GLM_LR_age_sex_immune_status_', format(now(), format = "%Y%m%d_%H%M"), ".png"), plot = S3_OR_age_sex_immune_status,
       width = 14, height = 5, dpi = 600)



