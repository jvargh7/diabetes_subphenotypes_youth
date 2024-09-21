
vl_column4 = "CBL"
vl_column5 = "VISIT"

data_path1 <- paste0(path_diabetes_subphenotypes_youth_folder,"/working/today/Data/sas7bdat")
data_path2 <- paste0(path_diabetes_subphenotypes_youth_folder,"/working/today2/Data")

labs_today <- data_extract(study_name,vl_column4,data_path1) 
visit_today <- data_extract(study_name,vl_column5,data_path1) 
visit_today2 <- data_extract(study_name,vl_column5,data_path2,df_name = "VISITFINAL") 

mvisit = left_join(labs_today,
                   visit_today %>% dplyr::select(-release_visit),
                   by=c("study_id","randdays"))

rm(labs_today,visit_today)
