
vl_column4 = "CBL"
vl_column5 = "VISIT"

data_path <-  "C:/Cloud/OneDrive - Emory University/Proposals/ADA Youth Phenotypes/working/today/Data/sas7bdat"
data_path2 <-  "C:/Cloud/OneDrive - Emory University/Proposals/ADA Youth Phenotypes/working/today2/Data"

labs_today <- data_extract(study_name,vl_column4,data_path) 
visit_today <- data_extract(study_name,vl_column5,data_path) 
visit_today2 <- data_extract(study_name,vl_column5,data_path2,df_name = "VISITFINAL") 

mvisit = left_join(labs_today,
                   visit_today %>% dplyr::select(-release_visit),
                   by=c("study_id","randdays"))

rm(labs_today,visit_today)
