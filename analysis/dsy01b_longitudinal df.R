library(haven)

search_mnsi <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/search/search_mnsi.RDS")) %>% 
  mutate(study_id = as.character(study_id), study = "SEARCH") 
today_mnsi <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/today/today_mnsi.RDS")) %>% 
  mutate(study = "TODAY") %>% 
  rename(wave = dataset)


analytic_df <- read.csv("analysis/dsy01_analytic sample.csv") %>% 
  left_join(search_mnsi) %>% 
  left_join(today_mnsi)

# no repeated measures in TODAY
repeated_df <- analytic_df %>%
  group_by(study_id,study) %>%
  dplyr::filter(n() > 1) %>%
  ungroup() 
