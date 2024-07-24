
vl_column1 = "BASELINE"
vl_column2 = "PAT"
vl_column3 = "PRIMOUT"

study_name = "TODAY"


data_path <- paste0(path_diabetes_subphenotypes_youth_folder,"/working/today/Data/sas7bdat")

baseline <- data_extract(study_name,vl_column1,data_path) 
pat <- data_extract(study_name,vl_column2,data_path) 
primout <- data_extract(study_name,vl_column3,data_path) 

baseline <- left_join(baseline,
                      pat,
                      by=c("study_id","randdays")) %>% 
  left_join(primout %>% 
              mutate(treatment = case_when(treatment == 1 ~ "Metformin",
                                           treatment == 2 ~ "Metformin + Rosiglitazone",
                                           treatment == 3 ~ "Metformin + Lifestyle"),
                     reasons_outcome = case_when(reasons_outcome == 1 ~ "HbA1c",
                                                 reasons_outcome == 2 ~ "Insulin",
                                                 reasons_outcome == 3 ~ "Metabolic decompensation",
                                                 TRUE ~ NA_character_)),
            by=c("study_id")) %>% 
  mutate(bmi = case_when(bmi == 1 ~ 32.000,
                         bmi == 3 ~ 38.000,
                         TRUE ~ bmi),
         bmipct = case_when(bmipct == 1 ~ "<98.5",
                            bmipct == 2 ~ ">=98.5",
                            TRUE ~ NA_character_),
         bmiz = case_when(bmiz == 1 ~ "<=2.20",
                          bmiz == 3 ~ ">=2.35",
                          bmiz > 2.20 ~ "2.21-2.34",
                          TRUE ~ NA_character_),
         age = case_when(age ==1 ~ "<=13",
                         age == 3 ~ ">15",
                         age %in% c(14,15) ~ "14-15",
                         TRUE ~ NA_character_),
         female = case_when(female == 1 ~ 1,
                            female == 2 ~ 0,
                            TRUE ~ NA_real_),
         dmduration = case_when(dmduration == 1 ~ "<=5 months",
                                dmduration == 2 ~ ">5 months",
                                TRUE ~ NA_character_),
         wc = case_when(wc == 1 ~ "<=100",
                        wc == 3 ~ ">=110",
                        wc >100 ~ "101-109",
                        TRUE ~ NA_character_))  %>% 
  mutate(race2 = case_when(race == 1 ~ "black",
                           race == 2 ~ "hispanic",
                           race == 3 ~ "white",
                           race == 4 ~ "other",
                           TRUE ~ NA_character_),
         ethnicity = case_when(race == 2 ~ "hispanic",
                               TRUE ~ "non-hispanic"),
         race_eth = case_when(race == 2 ~ "Hispanic",
                              race == 1 ~ "NH Black",
                              race == 3 ~ "NH White",
                              TRUE ~ "NH Other")) %>% 
  dplyr::select(-race) %>% 
  rename(race = race2)


rm(pat,primout)
