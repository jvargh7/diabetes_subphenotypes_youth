vl_pe1 = "PE"
vl_pe2 = "PEMD"

data_path1 <-  "C:/Cloud/OneDrive - Emory University/Proposals/ADA Youth Phenotypes/working/today/Data/sas7bdat"
data_path2 <-  "C:/Cloud/OneDrive - Emory University/Proposals/ADA Youth Phenotypes/working/today2/Data"

pe_today <- data_extract(study_name,vl_pe1,data_path1)  %>% 
  mutate_at(vars(heent,thyroid,lungs,heart),~case_when(. %in% c(1,2) ~ .-1,
                                                                  TRUE ~ NA_real_))
pe_today2 <- data_extract(study_name,vl_pe2,data_path2,df_name = "PEMDFINAL")  %>% 
  mutate_at(vars(heent,thyroid,lungs,heart,abdomen,extremeties, skin, neuro),~case_when(. %in% c(1,2) ~ .-1,
                                                                  . == 1 ~ 1,
                                                                  TRUE ~ NA_real_))
