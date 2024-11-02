rm(list=ls()); gc(); source(".Rprofile")

step1_search <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/search/search_etiologic.RDS")) %>% 
  dplyr::filter(age >= 10,age<20) %>% 
  mutate(study_id = as.character(study_id),
         study = "SEARCH",
         insulinf=NA) %>% 
  mutate(age_category = case_when(age < 14 ~ "<=13",
                                  age >15 ~ ">15",
                                  TRUE ~ "14-15"),
         dmduration_category = case_when(dmduration <= 5 ~ "<=5 months",
                                         TRUE ~ "6-12 months"))

step2_search <- step1_search %>% 
  dplyr::filter(dmduration >= 0 & dmduration <= duration_cutoff)

step3_search <- step2_search %>% 
  dplyr::filter(!is.na(bmi))  

# participants age: 10–17 y
today <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/today/today_baseline.RDS")) %>% 
  mutate(study = "TODAY", insulin = 0, metformin = 1) %>% 
  dplyr::filter(dmduration == "<=5 months") %>% 
  rename(age_category = age,
         dmduration_category = dmduration)



selected_vars = c("study_id","study","age_category","dmduration_category","race_eth",
                  "female","bmi","hba1c","cpeptidef", "sbp","dbp","ldlc","hdlc",
                  "tgl","glucosef","insulinf","totalc","insulin","metformin")


source_df = bind_rows(step3_search %>% dplyr::select(one_of(selected_vars)),
                      today %>% dplyr::select(one_of(selected_vars)))

step4_analytic = source_df %>% 
  dplyr::filter(!is.na(hba1c) | !is.na(cpeptidef) | !is.na(totalc) | !is.na(ldlc) | !is.na(hdlc))



# Comparison of analytic and excluded --------

source("functions/table1_summary.R")

overall_df = bind_rows(step2_search %>% dplyr::select(study_id,study,age_category,race_eth,female),
                       today %>% dplyr::select(study_id,study,age_category,race_eth,female)) %>% 
  mutate(analytic_category = case_when(study_id %in% step4_analytic$study_id ~ "Analytic",
                                       study_id %in% step3_search$study_id ~ "Lab Missing",
                                       study_id %in% step2_search$study_id ~ "BMI Missing",
                                       TRUE ~ NA_character_))

table(overall_df$analytic_category)


p_vars = c("female")
g_vars = c("age_category","race_eth","study")

table_df = overall_df %>% 
  table1_summary(.,p_vars = p_vars,g_vars = g_vars,id_vars = "analytic_category") %>% 
  dplyr::select(analytic_category,group,variable,est,value) %>% 
  pivot_wider(names_from=est,values_from=value) %>% 
  mutate(output = paste0(round(freq,0)," (",round(proportion,1),"%)")) %>% 
  dplyr::select(variable,group,analytic_category,output) %>% 
  pivot_wider(names_from=analytic_category,values_from=output) %>% 
  dplyr::select(variable,group,Analytic,'BMI Missing','Lab Missing')

write_csv(table_df,"etiologic/paper/table_flowchart descriptive characteristics.csv")



