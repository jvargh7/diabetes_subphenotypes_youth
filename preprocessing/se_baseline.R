
source("preprocessing/sepre01_search1to3 variables.R")
source("preprocessing/sepre02_search4 variables.R")

baseline <- search1to3 %>% mutate(wave = "SEARCH 1 TO 3") %>%
  dplyr::filter(dtype_factorial %in% c(3,4,6,7,9,10)) %>% 
  # TYPE_DAASENS_2 classifies as
  # T1: 1, 2, 3, 7, 8
  # T2: 4
  # MODY: 5
  # OTHER/UNKNOWN: 6, 9, 10
  
  
  bind_rows(search4 %>% mutate(wave = "SEARCH 4") %>% 
              dplyr::filter(study_id %in% .$study_id)) %>% 
  
  arrange(study_id,age) %>% 
  group_by(study_id) %>% 
  
  mutate_at(vars(female,dmagediag),function(x) zoo::na.locf(x,na.rm=FALSE)) %>% 
  dplyr::filter(age == min(age)) %>% 
  dplyr::filter(!is.na(bmi)) %>% 
  ungroup() %>% 
  mutate(dmduration = dmduration/12,
         female = case_when(female == "F" ~ 1,
                            female == "M" ~ 0,
                            TRUE ~ NA_real_))

saveRDS(baseline,paste0(path_search_folder,"/search_baseline.RDS"))
