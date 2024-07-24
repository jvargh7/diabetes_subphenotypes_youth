
today_baseline <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/today/today_baseline.RDS"))
source("preprocessing/tdypre02_run dataset.R")
source("preprocessing/tdypre03_mvisit datasets.R")
source("preprocessing/tdypre05_pe datasets.R")


today_multimorbidity = bind_rows(pe_today %>% mutate(dataset = "TODAY"),
                       pe_today2 %>% mutate(dataset="TODAY2")) %>% 
  dplyr::select(-release_visit) %>% 
  left_join(
    bind_rows(mvisit %>% dplyr::filter(release_visit != "M00"),
              today_baseline %>% dplyr::select(-wc,-bmipct,-bmiz),
              visit_today2),
    by = c("randdays","study_id")
    
  ) %>% 
  arrange(study_id,randdays) %>% 
  group_by(study_id) %>% 
  mutate_at(vars(treatment,primary_outcome,reasons_outcome,
                 daystooutcome_start, daystooutcome_end, daystocensor),function(x) zoo::na.locf(x,na.rm=FALSE)) %>% 
  
  dplyr::select(study_id,randdays,release_visit, dataset,
                treatment, primary_outcome, reasons_outcome,daystooutcome_start, daystooutcome_end, daystocensor,
                heent,thyroid,lungs,heart, abdomen, extremeties, skin, neuro,
                htndrug, lipiddrug, steroiddrug, weightlossdrug,
                hba1c:tgl
                )
saveRDS(today_multimorbidity,paste0(path_diabetes_subphenotypes_youth_folder,"/working/today/today_multimorbidity.RDS"))
