rm(list=ls()); gc(); source(".Rprofile")

source("preprocessing/tdypre01_baseline datasets.R")
source("preprocessing/tdypre02_run dataset.R")
source("preprocessing/tdypre03_mvisit datasets.R")

today_baseline <- baseline %>% 
  dplyr::select(-randdays) %>% 
  left_join(mvisit %>% 
              dplyr::filter(release_visit == "M00") %>% 
              dplyr::select(-bmi,-sbp,-dbp,-wc,-bmipct,-bmiz,
                            -htndrug,-lipiddrug,-steroiddrug,-weightlossdrug),
            by = "study_id") 


saveRDS(today_baseline,paste0(path_diabetes_subphenotypes_youth_folder,"/working/today/today_baseline.RDS"))
