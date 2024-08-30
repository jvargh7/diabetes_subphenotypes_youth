rm(list=ls()); gc(); source(".Rprofile")

# library(anthroplus)

search <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/search/search_etiologic.RDS")) %>% 
  dplyr::filter(age >= 10,age<20) %>% 
  dplyr::filter(dmduration >= 0 & dmduration <= duration_cutoff) %>% 
  mutate(study_id = as.character(study_id),study = "SEARCH",insulinf=NA) %>% 
  mutate(age_category = case_when(age < 14 ~ "<=13",
                                  age >15 ~ ">15",
                                  TRUE ~ "14-15"),
         dmduration_category = case_when(dmduration <= 5 ~ "<=5 months",
                                         TRUE ~ "6-12 months"))

# 19
selected_vars = c("study_id","study","age_category","dmduration_category","race_eth",
                  "female","bmi","hba1c","cpeptidef", "sbp","dbp","ldlc","hdlc",
                  "tgl","glucosef","insulinf","totalc","insulin","metformin")


source_df = search %>% dplyr::select(one_of(selected_vars))

#write.csv(source_df, paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/etiologic/setdy01c_source sample.csv"))

analytic_df = source_df %>% 
    dplyr::filter(!is.na(hba1c) | !is.na(cpeptidef) | !is.na(totalc) | !is.na(ldlc) | !is.na(hdlc))

write.csv(analytic_df, paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/etiologic/search only/sepre01_analytic sample.csv"))

complete_cases_df <- source_df %>% 
  dplyr::select(-c("insulinf","glucosef")) %>% 
  dplyr::filter(complete.cases(.)) # N = 335

#write.csv(complete_cases_df, paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/etiologic/setdy01b_complete cases.csv"))

