rm(list=ls()); gc(); source(".Rprofile")

source("preprocessing/sepre01_search1to3 variables.R")
source("preprocessing/sepre02_search4 variables.R")


baseline <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/search/search_baseline.RDS"))

search_multimorbidity <- bind_rows(search1to3 %>% mutate(dataset = "SEARCH 1 TO 3"),
                         search4 %>% mutate(dataset = "SEARCH 4")) %>% 
  
  mutate(type_diabetes = case_when(study_id %in% baseline$study_id ~ "T2DM",
                                   TRUE ~ "T1DM or MODY or HYBRID")) %>% 
  arrange(study_id,age) %>% 
  group_by(study_id) %>% 
  mutate_at(vars(female,dmagediag),function(x) zoo::na.locf(x,na.rm=FALSE)) %>% 
  dplyr::select(study_id, dataset, type_diabetes, dmduration,
                dmagediag, age, female, race,
                bmi, sbp, dbp, wc, hba1c,
                totalc,ldlc, hdlc, vldlc, tgl,
                htndrug, lipiddrug, insulin,
                ends_with("_diag"),
                hypertension,
                smokecount, ecigarette_dayslast30,
                marijuana_dayslast30, opioid_prescription,
                otherdrug_ever, matches("^med[0-9]+")
                ) %>% 

  mutate(highbp_diag = case_when(hypertension == 1 ~ 1,
                                 htndrug == 1 ~ 1,
                                 TRUE ~ highbp_diag),
         highchol_diag = case_when(lipiddrug == 1 ~ 1,
                                   TRUE ~ highchol_diag)) %>% 
  dplyr::select(-hypertension,-htndrug,-lipiddrug,-insulin) 

saveRDS(search_multimorbidity,paste0(path_diabetes_subphenotypes_youth_folder,"/working/search/search_multimorbidity.RDS"))
