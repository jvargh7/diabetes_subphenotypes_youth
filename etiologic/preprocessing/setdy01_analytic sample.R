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
                                         TRUE ~ "6-12 months")) %>% 
  mutate(bmipct = case_when((pnorm(bmiz) * 100) < 98.5 ~ "<98.5",
                            TRUE ~ ">=98.5"))

# participants age: 10–17 y
today <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/today/today_baseline.RDS")) %>% 
  mutate(study = "TODAY", insulin = 0, metformin = 1) %>% 
  dplyr::filter(dmduration == "<=5 months") %>% 
  rename(age_category = age,
         dmduration_category = dmduration)


# 19
selected_vars = c("study_id","study","age_category","dmduration_category","race_eth",
                  "female","bmi","hba1c","cpeptidef", "sbp","dbp","ldlc","hdlc","bmipct",
                  "tgl","glucosef","insulinf","totalc","insulin","metformin","dmfamilyhistory",
                  "retinopathy_lefteye","retinopathy_righteye","retinopathy","retinopathy_tx",
                  "dkd","nephropathy_prescription","nephropathy_tx","nephropathy_diag")


source_df = bind_rows(search %>% dplyr::select(one_of(selected_vars)),
                      today %>% dplyr::select(one_of(selected_vars)))

write.csv(source_df, paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/etiologic/setdy01c_source sample.csv"))

analytic_df = source_df %>% 
    dplyr::filter(!is.na(hba1c) | !is.na(cpeptidef) | !is.na(totalc) | !is.na(ldlc) | !is.na(hdlc))

write.csv(analytic_df, paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/etiologic/setdy01a_analytic sample.csv"))

complete_cases_df <- source_df %>% 
  dplyr::select(-c("insulinf","glucosef")) %>% 
  dplyr::filter(complete.cases(.)) # N = 335

write.csv(complete_cases_df, paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/etiologic/setdy01b_complete cases.csv"))

