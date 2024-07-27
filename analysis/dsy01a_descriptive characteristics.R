rm(list=ls());gc();source(".Rprofile")


### Table 1 - total
analytic_dataset_cluster <- read.csv(paste0(path_diabetes_subphenotypes_youth_folder, '/working/cleaned/setdy03_kmeans clustering.csv'))

source("functions/table1_summary.R")

names(analytic_dataset_cluster)

c_vars = c("bmi","hba1c","cpeptidef", "sbp","dbp","ldlc","hdlc","totalc","insulinf","tgl","glucosef")
p_vars = c("female","insulin","metformin")
g_vars = c("cluster","study","dmduration_category","age_category","race_eth")

table_df = analytic_dataset_cluster %>% 
  bind_rows(.,
            {.} %>% 
              mutate(cluster="Total")) %>% 
  table1_summary(.,c_vars = c_vars,p_vars = p_vars,g_vars = g_vars,id_vars = "cluster")

write_csv(table_df,"analysis/dsy01a_descriptive characteristics - total by cluster.csv")


### Table 1 by clusters

mean_vars = c("bmi","cpeptidef", "sbp","dbp","ldlc","hdlc","totalc","insulinf","tgl","glucosef")

median_vars = c("hba1c")

table_df <- read_csv("analysis/dsy01a_descriptive characteristics - total by cluster.csv") %>% 
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
  dplyr::select(variable,group,Total,OB,ID,IR)

write_csv(table_df,"paper/table_descriptive characteristics by cluster.csv")  







