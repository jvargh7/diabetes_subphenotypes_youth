rm(list=ls()); gc(); source(".Rprofile")


search <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/search/search_baseline.RDS")) %>% 
  dplyr::filter(age >= 10,age<20) %>% 
  dplyr::filter(dmduration >= 0 & dmduration <= duration_cutoff) %>% 
  mutate(study_id = as.character(study_id)) %>% 
  mutate(age_category = case_when(age <= 13 ~ "=<13",
                                  age >15 ~ ">15",
                                  TRUE ~ "14-15"),
         dmduration_category = case_when(dmduration <= 5 ~ "<=5 months",
                                         TRUE ~ "6-12 months"))
# participants age: 10–17 y
today <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/today/today_baseline.RDS")) %>% 
  mutate(study = "TODAY") %>% 
  dplyr::filter(dmduration == "<=5 months") %>% 
  rename(age_category = age,
         dmduration_category = dmduration)


selected_vars = c("study_id","age_category","dmduration_category",
                  "female","bmi","hba1c","cpeptidef",
                  "sbp","dbp","totalc","ldlc","hdlc")


source_df = bind_rows(search %>% dplyr::select(one_of(selected_vars)) %>% mutate(study = "SEARCH"),
                      today %>% dplyr::select(one_of(selected_vars)) %>% mutate(study = "TODAY"))

analytic_df = source_df %>% 
    dplyr::filter(!is.na(hba1c) | !is.na(cpeptidef) | !is.na(totalc) | !is.na(ldlc) | !is.na(hdlc))

write_csv(analytic_df,"analysis/dsy01_analytic sample.csv")

complete_cases_df <- source_df[complete.cases(source_df), ] # N = 683

write.csv(complete_cases_df, "analysis/dsy01_complete cases df.csv", row.names = FALSE)
