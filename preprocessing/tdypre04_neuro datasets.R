vl_neuro1 = "NEURO"
vl_neuro2 = "MNSIFINAL"

data_path1 <- paste0(path_diabetes_subphenotypes_youth_folder,"/working/today/Data/sas7bdat")
data_path2 <- paste0(path_diabetes_subphenotypes_youth_folder,"/working/today2/Data")

neuro_today <- data_extract(study_name,vl_neuro1,data_path1) 
neuro_today2 <- data_extract(study_name,vl_neuro2,data_path2,df_name = "MNSIFINAL") 




