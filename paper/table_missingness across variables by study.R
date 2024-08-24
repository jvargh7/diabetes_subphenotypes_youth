rm(list=ls()); gc(); source(".Rprofile")

source("functions/calculate_na_summary.R")

analytic_df <- read.csv(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/setdy01a_analytic sample.csv"))

### cluster variables
vars_of_interest <- c("bmi", "hba1c", "cpeptidef", "sbp", "dbp", "ldlc", "hdlc")

na_summary_all <- calculate_na_summary(analytic_df) %>%
  mutate(Study = "All")
na_summary_SEARCH <- calculate_na_summary(analytic_df, "SEARCH") %>%
  mutate(Study = "SEARCH")
na_summary_TODAY <- calculate_na_summary(analytic_df, "TODAY") %>%
  mutate(Study = "TODAY")


combined_summary <- bind_rows(na_summary_all, na_summary_SEARCH, na_summary_TODAY) %>%
  pivot_wider(names_from = Study, values_from = c(NA_Count, NA_Percentage))

#-------------------------------------------------------------------------------------------------------------------------------

crosssec_df <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/dsy01a_cross sectional df.RDS")) %>% 
  dplyr::select(-nonna_selfmnsi) %>% 
  dplyr::filter(!is.na(combined_abnormal)) 

### MNSI variables
vars_of_interest <- names(crosssec_df)[grep("mnsi", names(crosssec_df))]

na_summary_all <- calculate_na_summary(crosssec_df) %>%
  mutate(Study = "All")
na_summary_SEARCH <- calculate_na_summary(crosssec_df, "SEARCH") %>%
  mutate(Study = "SEARCH")
na_summary_TODAY <- calculate_na_summary(crosssec_df, "TODAY") %>%
  mutate(Study = "TODAY")

combined_summary <- bind_rows(na_summary_all, na_summary_SEARCH, na_summary_TODAY) %>%
  pivot_wider(names_from = Study, values_from = c(NA_Count, NA_Percentage))



