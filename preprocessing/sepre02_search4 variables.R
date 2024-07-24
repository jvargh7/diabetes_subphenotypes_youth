
vl_column = "SEARCH4"
study_name = "SEARCH"

data_path <- paste0(path_diabetes_subphenotypes_youth_folder,"/working/search/SEARCH_V1/Data/SEARCH 4")

search4 <- data_extract(study_name,vl_column,data_path)  %>% 
  mutate(race2 = case_when(race %in% c("black","white") ~ race,
                           TRUE ~ "other"),
         ethnicity = case_when(race == "Hispanic" ~ "hispanic",
                               TRUE ~ "non-hispanic"),
         race_eth = case_when(race == "hispanic" ~ "Hispanic",
                              race == "black" ~ "NH Black",
                              race == "white" ~ "NH White",
                              TRUE ~ "NH Other"),
         female = case_when(female %in% c("F","f") ~ 1,
                            TRUE ~ 0)) %>% 
  dplyr::select(-race) %>% 
  rename(race = race2) %>% 
  mutate_at(vars(ends_with("_diag"),htndrug,lipiddrug),~case_when(. == 2 ~ 0,
                                          . == 1 ~ 1,
                                          TRUE ~ NA_real_))
