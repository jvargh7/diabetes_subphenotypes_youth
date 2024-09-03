
source("preprocessing/tdypre04_neuro datasets.R")

today_baseline <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/today/today_baseline.RDS"))


today_mnsi = bind_rows(neuro_today %>% mutate(dataset = "TODAY"),
                  neuro_today2 %>% mutate(dataset="TODAY2")) %>% 
  mutate(nonna_selfmnsi = apply(.[,regexpr(colnames(.),pattern = "selfmnsi")>0],1,function(x) sum(!is.na(x)))) %>%
  dplyr::filter(nonna_selfmnsi > 0) %>% 
  dplyr::filter(study_id %in% today_baseline$study_id) %>% 
  arrange(study_id,randdays) %>% 
  mutate_at(vars(matches("selfmnsi[0-9]+")),function(x) case_when(x == 1 ~ 1,
                                                                  x == 0 ~ 0,
                                                                  TRUE ~ NA_real_)) %>% 
  mutate_at(vars(obsmnsil,obsmnsir),function(x) case_when(x == 1 ~ "Normal",
                                                          x == 2 ~ "Abnormal",
                                                          TRUE ~ NA_character_)) %>% 
  mutate_at(vars(matches("obsmnsi(r|l)[0-9]")),function(x) case_when(x == 1 ~ 1,
                                                                     TRUE ~ 0)) %>% 
  mutate_at(vars(matches("_ulcer")),function(x) case_when(x == 1 ~ 1,
                                                          TRUE ~ 0))  %>% 
  mutate_at(vars(matches("_(reflex|perception|filament)")),function(x) case_when(x == 1 ~ "Present",
                                                                                 x == 2 ~ "Reduced",
                                                                                 x == 3 ~ "Absent",
                                                                                 TRUE ~ NA_character_)) 

saveRDS(today_mnsi,paste0(path_diabetes_subphenotypes_youth_folder,"/working/today/today_mnsi.RDS"))
today_mnsi <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/today/today_mnsi.RDS"))
