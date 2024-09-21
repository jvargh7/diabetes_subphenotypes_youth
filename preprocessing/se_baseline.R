rm(list=ls()); gc(); source(".Rprofile")

source("preprocessing/sepre01_search1to3 variables.R")
source("preprocessing/sepre02_search4 variables.R")




etiologic <- search1to3 %>% mutate(wave = "SEARCH 1 TO 3") %>%
  # dplyr::filter(dtype_factorial %in% c(3,4,6,7,9,10)) %>% 
  # TYPE_DAASENS_2 classifies as
  # T1: 1, 2, 3, 7, 8
  # T2: 4
  # MODY: 5
  # OTHER/UNKNOWN: 6, 9, 10
  dplyr::filter(dtype_etiologic %in% c("T2: DAA neg & ins resistant (4)",
                                       "Other/Unknown (6,9,10)")) %>% 
  
  bind_rows(search4 %>% mutate(wave = "SEARCH 4") %>% 
              dplyr::filter(study_id %in% .$study_id)) %>% 
  
  arrange(study_id,age) %>% 
  group_by(study_id) %>% 
  
  mutate_at(vars(female,dmagediag),function(x) zoo::na.locf(x,na.rm=FALSE)) %>% 
  dplyr::filter(age == min(age)) %>% 
  dplyr::filter(!is.na(bmi)) %>% 
  ungroup() %>% 
  mutate(dmduration = dmduration/12) %>% 
  mutate(retinopathy = if_else(is.na(retinopathy_lefteye) & is.na(retinopathy_righteye), 0, 1))

saveRDS(etiologic,paste0(path_diabetes_subphenotypes_youth_folder,"/working/search/search_etiologic.RDS"))



# SEARCH_FACTORIAL ---------
factorial <- search1to3 %>% mutate(wave = "SEARCH 1 TO 3") %>%
  dplyr::filter(dtype_factorial %in% c(3,4,6,7,9,10)) %>%
  # TYPE_DAASENS_2 classifies as
  # T1: 1, 2, 3, 7, 8
  # T2: 4
  # MODY: 5
  # OTHER/UNKNOWN: 6, 9, 10
  # dplyr::filter(dtype_etiologic %in% c("T2: DAA neg & ins resistant (4)",
  #                                      "Other/Unknown (6,9,10)")) %>% 
  # 
  bind_rows(search4 %>% mutate(wave = "SEARCH 4") %>% 
              dplyr::filter(study_id %in% .$study_id)) %>% 
  
  arrange(study_id,age) %>% 
  group_by(study_id) %>% 
  
  mutate_at(vars(female,dmagediag),function(x) zoo::na.locf(x,na.rm=FALSE)) %>% 
  dplyr::filter(age == min(age)) %>% 
  dplyr::filter(!is.na(bmi)) %>% 
  ungroup() %>% 
  mutate(dmduration = dmduration/12)

saveRDS(factorial,paste0(path_diabetes_subphenotypes_youth_folder,"/working/search/search_factorial.RDS"))


# SEARCH_PROV ---------
prov <- search1to3 %>% mutate(wave = "SEARCH 1 TO 3") %>%
  dplyr::filter(dtype_prov %in% c("2. TYPE 2","4. OTH/UNK")) %>%
  # TYPE_DAASENS_2 classifies as
  # T1: 1, 2, 3, 7, 8
  # T2: 4
  # MODY: 5
  # OTHER/UNKNOWN: 6, 9, 10
  # dplyr::filter(dtype_etiologic %in% c("T2: DAA neg & ins resistant (4)",
  #                                      "Other/Unknown (6,9,10)")) %>% 
  # 
  bind_rows(search4 %>% mutate(wave = "SEARCH 4") %>% 
              dplyr::filter(study_id %in% .$study_id)) %>% 
  
  arrange(study_id,age) %>% 
  group_by(study_id) %>% 
  
  mutate_at(vars(female,dmagediag),function(x) zoo::na.locf(x,na.rm=FALSE)) %>% 
  dplyr::filter(age == min(age)) %>% 
  dplyr::filter(!is.na(bmi)) %>% 
  ungroup() %>% 
  mutate(dmduration = dmduration/12)

saveRDS(prov,paste0(path_diabetes_subphenotypes_youth_folder,"/working/search/search_prov.RDS"))


