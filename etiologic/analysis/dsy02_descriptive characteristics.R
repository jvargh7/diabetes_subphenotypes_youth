rm(list=ls());gc();source(".Rprofile")


analytic_df <- read.csv(paste0(path_diabetes_subphenotypes_youth_folder, '/working/cleaned/etiologic/setdy04_kmeans clustering.csv')) 
source_df <- read.csv(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/etiologic/setdy01c_source sample.csv"))

source("functions/table1_summary.R")

c_vars = c("bmi","hba1c","cpeptidef", "sbp","dbp","ldlc","hdlc","totalc","insulinf","tgl","glucosef")
p_vars = c("female","insulin","metformin")
g_vars = c("cluster","study","dmduration_category","age_category","race_eth","bmipct")

#-----------------------------------------------------------------------------------------------------------
### Table 1 by clusters

table_df = analytic_df %>% 
  bind_rows(.,
            {.} %>% 
              mutate(cluster="Total")) %>% 
  table1_summary(.,c_vars = c_vars,p_vars = p_vars,g_vars = g_vars,id_vars = "cluster")

write_csv(table_df,"etiologic/analysis/dsy02_descriptive characteristics - total by cluster.csv")

#-----------------------------------------------------------------------------------------------------------
### STable 1 - included | excluded

g_vars = c("study","dmduration_category","age_category","race_eth","bmipct")

table_df = source_df %>% 
  mutate(study_cca = case_when(
    study == "SEARCH" & (study_id %in% analytic_df$study_id) ~ "search_included",
    study == "SEARCH" & !(study_id %in% analytic_df$study_id) ~ "search_excluded",
    study == "TODAY" & (study_id %in% analytic_df$study_id) ~ "today_included",
    study == "TODAY" & !(study_id %in% analytic_df$study_id) ~ "today_excluded"
  )) %>% 
  bind_rows(.,
            {.} %>% 
              mutate(study_cca="Total")) %>% 
  table1_summary(.,c_vars = c_vars,p_vars = p_vars,g_vars = g_vars,id_vars = "study_cca")

write_csv(table_df,"etiologic/analysis/dsy02_descriptive characteristics - total by study and cca.csv")


#-----------------------------------------------------------------------------------------------------------
### STable 2 - study*sex

g_vars = c("cluster","study","dmduration_category","age_category","race_eth","bmipct")

table_df = analytic_df %>% 
  mutate(study_sex = case_when(female == 1 & study == "SEARCH" ~ "Female_search",
                               female == 0 & study == "SEARCH" ~ "Male_search",
                               female == 1 & study == "TODAY" ~ "Female_today",
                               female == 0 & study == "TODAY" ~ "Male_today")) %>% 
  bind_rows(.,
            {.} %>% 
              mutate(study_sex="Total")) %>% 
  table1_summary(.,c_vars = c_vars,p_vars = p_vars,g_vars = g_vars,id_vars = "study_sex")

write_csv(table_df,"etiologic/analysis/dsy02_descriptive characteristics - total by study and sex.csv")


#-----------------------------------------------------------------------------------------------------------
### STable 3 - sex*cluster

table_df = analytic_df %>% 
  mutate(sex_cluster = case_when(female == 1 & cluster == "yMOD" ~ "Female_yMOD",
                                 female == 0 & cluster == "yMOD" ~ "Male_yMOD",
                                 female == 1 & cluster == "ySIDD" ~ "Female_ySIDD",
                                 female == 0 & cluster == "ySIDD" ~ "Male_ySIDD",
                                 female == 1 & cluster == "ySIRD" ~ "Female_ySIRD",
                                 female == 0 & cluster == "ySIRD" ~ "Male_ySIRD")) %>% 
  bind_rows(.,
            {.} %>% 
              mutate(sex_cluster="Total")) %>% 
  table1_summary(.,c_vars = c_vars,p_vars = p_vars,g_vars = g_vars,id_vars = "sex_cluster")

write_csv(table_df,"etiologic/analysis/dsy02_descriptive characteristics - total by sex and cluster.csv")

#-----------------------------------------------------------------------------------------------------------
### STable 4 - study

table_df = analytic_df %>% 
  bind_rows(.,
            {.} %>% 
              mutate(study="Total")) %>% 
  table1_summary(.,c_vars = c_vars,p_vars = p_vars,g_vars = g_vars,id_vars = "study")

write_csv(table_df,"etiologic/analysis/dsy02_descriptive characteristics - total by study.csv")

