source("preprocessing/sepre01_search1to3 variables.R")
source("preprocessing/sepre02_search4 variables.R")


baseline <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/search/search_baseline.RDS"))

search_mnsi <- bind_rows(search1to3 %>% mutate(wave = "SEARCH 1 TO 3"),
                         search4 %>% mutate(wave = "SEARCH 4")) %>% 
  mutate(nonna_selfmnsi = apply(.[,regexpr(colnames(.),pattern = "selfmnsi")>0],1,function(x) sum(!is.na(x)))) %>%
  dplyr::filter(nonna_selfmnsi > 0) %>% 
  dplyr::select(study_id,wave,dmduration,age,contains("mnsi")) %>% 
  dplyr::filter(study_id %in% baseline$study_id) %>% 
  arrange(study_id,age)  %>% 
  mutate_at(vars(matches("selfmnsi[0-9]+")),function(x) case_when(x == 1 ~ 0,
                                                                  x == 2 ~ 1,
                                                                  TRUE ~ NA_real_)) %>% 
  mutate_at(vars(obsmnsil,obsmnsir),function(x) case_when(x == 1 ~ "Abnormal",
                                                          x == 2 ~ "Normal",
                                                          TRUE ~ NA_character_)) %>% 
  mutate_at(vars(matches("obsmnsi(r|l)[0-9]")),function(x) case_when(x == 1 ~ 1,
                                                                     TRUE ~ 0)) %>% 
  mutate_at(vars(matches("_ulcer")),function(x) case_when(x == 1 ~ 1,
                                                          TRUE ~ 0))  %>% 
  mutate_at(vars(matches("_(reflex|perception|filament)")),function(x) case_when(x == 1 ~ "Present",
                                                                        x == 2 ~ "Reduced",
                                                                        x == 3 ~ "Absent",
                                                                        TRUE ~ NA_character_)) 

saveRDS(search_mnsi,paste0(path_diabetes_subphenotypes_youth_folder,"/working/search/search_mnsi.RDS"))
