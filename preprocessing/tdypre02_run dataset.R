vl_column3 = "RUN"

data_path <-  "C:/Cloud/OneDrive - Emory University/Proposals/ADA Youth Phenotypes/working/today/Data/sas7bdat"

run_today <- data_extract(study_name,vl_column3,data_path)  %>% 
  mutate(bmi = case_when(bmi == 1 ~ 32.000,
                         bmi == 3 ~ 38.000,
                         TRUE ~ bmi),
         bmipct = case_when(bmipct == 1 ~ "<98.5",
                            bmipct == 2 ~ ">=98.5",
                            TRUE ~ NA_character_),
         bmiz = case_when(bmiz == 1 ~ "<=2.20",
                          bmiz == 3 ~ ">=2.35",
                          bmiz > 2.20 ~ "2.21-2.34",
                          TRUE ~ NA_character_)) 
