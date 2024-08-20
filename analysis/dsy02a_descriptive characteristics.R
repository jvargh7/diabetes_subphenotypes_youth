rm(list=ls());gc();source(".Rprofile")


### Table 1 - total
analytic_df <- read.csv(paste0(path_diabetes_subphenotypes_youth_folder, '/working/cleaned/setdy03_kmeans clustering.csv')) 

source("functions/table1_summary.R")

names(analytic_df)

# analytic sample -----------------------------------------------------------------------------------------------------------
c_vars = c("bmi","hba1c","cpeptidef", "sbp","dbp","ldlc","hdlc","totalc","insulinf","tgl","glucosef")
p_vars = c("female","insulin","metformin")
g_vars = c("cluster","study","dmduration_category","age_category","race_eth")

table_df = analytic_df %>% 
  bind_rows(.,
            {.} %>% 
              mutate(cluster="Total")) %>% 
  table1_summary(.,c_vars = c_vars,p_vars = p_vars,g_vars = g_vars,id_vars = "cluster")

write_csv(table_df,"analysis/dsy02a_descriptive characteristics - total by cluster.csv")


### Table 1 by clusters

mean_vars = c("bmi","cpeptidef", "sbp","dbp","ldlc","hdlc","totalc","insulinf","tgl","glucosef")

median_vars = c("hba1c")

table_df <- read_csv("analysis/dsy02a_descriptive characteristics - total by cluster.csv") %>% 
  dplyr::mutate(selected_rows = case_when(variable %in% mean_vars & est %in% c("mean","sd") ~ 1,
                                          variable %in% median_vars & est %in% c("median","q25","q75") ~ 1,
                                          !variable %in% c(mean_vars,median_vars) ~ 1,
                                          TRUE ~ 0
  )) %>% 
  dplyr::filter(selected_rows == 1) %>% 
  dplyr::select(cluster,group,variable,est,value) %>% 
  pivot_wider(names_from=est,values_from=value) %>% 
  mutate(output = case_when(variable %in% mean_vars ~ paste0(round(mean,1)," (",round(sd,1),")"),
                            variable %in% median_vars ~ paste0(round(median,1)," (",round(q25,1),", ",round(q75,1),")"),
                            TRUE ~ paste0(round(freq,0)," (",round(proportion,1),"%)")
  )) %>% 
  dplyr::select(variable,group,cluster,output) %>% 
  pivot_wider(names_from=cluster,values_from=output) %>% 
  dplyr::select(variable,group,Total,yMOD,ySIDD,ySIRD)

write_csv(table_df,"paper/table_descriptive characteristics by cluster.csv")  


# cross-sectional -----------------------------------------------------------------------------------------------------------

crossec_df <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/dsy01a_cross sectional df.RDS")) %>% 
  mutate(cs_unavailable = case_when(is.na(combined_abnormal) ~ 1,
                                  TRUE ~ 0))

all_na <- crossec_df %>% 
  dplyr::filter(is.na(combined_abnormal)) %>% 
  summarise(n_distinct(study_id))

table(crossec_df$cs_unavailable,crossec_df$cluster)
table(crossec_df$survey_abnormal,crossec_df$cluster)
table(crossec_df$exam_abnormal,crossec_df$cluster)
table(crossec_df$combined_abnormal,crossec_df$cluster)

prop.table(table(crossec_df$cs_unavailable,crossec_df$cluster))
prop.table(table(crossec_df$survey_abnormal,crossec_df$cluster))
prop.table(table(crossec_df$exam_abnormal,crossec_df$cluster))
prop.table(table(crossec_df$combined_abnormal,crossec_df$cluster))

# Code below not getting what I want, don't know why

# p_vars = c("cs_unavailable","survey_abnormal","exam_abnormal","combined_abnormal")
# 
# table_df1 = crossec_df %>% 
#   bind_rows(.,
#             {.} %>% 
#               mutate(cluster="Total")) %>% 
#   table1_summary(.,p_vars = p_vars,id_vars = "cluster")
# 
# 
# table_df <- table_df1 %>% 
#   dplyr::select(cluster,variable,est,value) %>% 
#   pivot_wider(names_from=est,values_from=value) %>% 
#   mutate(output = paste0(round(freq,0)," (",round(proportion,1),"%)")) %>% 
#   dplyr::select(variable,cluster,output) %>% 
#   pivot_wider(names_from=cluster,values_from=output) %>% 
#   dplyr::select(variable,Total,MOD,SIDD,SIRD)

# longitudinal -----------------------------------------------------------------------------------------------------------

longitudinal_df <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/dsy01b_longitudinal df.RDS")) %>% 
  mutate(cs_unavailable = case_when(is.na(combined_abnormal) ~ 1,
                                  TRUE ~ 0)) 

all_na <- longitudinal_df %>% 
  dplyr::filter(is.na(combined_abnormal)) %>% 
  summarise(n_distinct(study_id))

age_diff_stats <- longitudinal_df %>%
  group_by(cluster) %>%
  summarise(
    mean_age_diff = mean(age_diff, na.rm = TRUE),
    sd_age_diff = sd(age_diff, na.rm = TRUE)
  ) %>% 
  ungroup()

prop.table(table(longitudinal_df$cs_unavailable,longitudinal_df$cluster))
prop.table(table(longitudinal_df$survey_abnormal,longitudinal_df$cluster))
prop.table(table(longitudinal_df$exam_abnormal,longitudinal_df$cluster))
prop.table(table(longitudinal_df$combined_abnormal,longitudinal_df$cluster))
