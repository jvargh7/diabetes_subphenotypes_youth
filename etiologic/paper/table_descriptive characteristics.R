rm(list=ls());gc();source(".Rprofile")


### Table 1 by clusters

mean_vars = c("bmi","cpeptidef", "sbp","dbp","ldlc","hdlc","totalc","insulinf","tgl","glucosef")

median_vars = c("hba1c")

table_df <- read_csv("etiologic/analysis/dsy02_descriptive characteristics - total by cluster.csv") %>% 
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

write_csv(table_df,"etiologic/paper/table_descriptive characteristics by cluster.csv")  

#--------------------------------------------------------------------------------------------------------------------------
## STable 1 - included | excluded

table_df <- read_csv("etiologic/analysis/dsy02_descriptive characteristics - total by study and cca.csv") %>% 
  dplyr::mutate(selected_rows = case_when(variable %in% mean_vars & est %in% c("mean","sd") ~ 1,
                                          variable %in% median_vars & est %in% c("median","q25","q75") ~ 1,
                                          !variable %in% c(mean_vars,median_vars) ~ 1,
                                          TRUE ~ 0
  )) %>% 
  dplyr::filter(selected_rows == 1) %>% 
  dplyr::select(study_cca,group,variable,est,value) %>% 
  pivot_wider(names_from=est,values_from=value) %>% 
  mutate(output = case_when(variable %in% mean_vars ~ paste0(round(mean,1)," (",round(sd,1),")"),
                            variable %in% median_vars ~ paste0(round(median,1)," (",round(q25,1),", ",round(q75,1),")"),
                            TRUE ~ paste0(round(freq,0)," (",round(proportion,1),"%)")
  )) %>% 
  dplyr::select(variable,group,study_cca,output) %>% 
  pivot_wider(names_from=study_cca,values_from=output) %>% 
  dplyr::select(variable,group,Total,search_included,search_excluded,today_included)

write_csv(table_df,"etiologic/paper/table_descriptive characteristics by study and cca.csv")  


#-----------------------------------------------------------------------------------------------------------
### STable 2 - study*sex

table_df <- read_csv("etiologic/analysis/dsy02_descriptive characteristics - total by study and sex.csv") %>% 
  dplyr::mutate(selected_rows = case_when(variable %in% mean_vars & est %in% c("mean","sd") ~ 1,
                                          variable %in% median_vars & est %in% c("median","q25","q75") ~ 1,
                                          !variable %in% c(mean_vars,median_vars) ~ 1,
                                          TRUE ~ 0
  )) %>% 
  dplyr::filter(selected_rows == 1) %>% 
  dplyr::select(study_sex,group,variable,est,value) %>% 
  pivot_wider(names_from=est,values_from=value) %>% 
  mutate(output = case_when(variable %in% mean_vars ~ paste0(round(mean,1)," (",round(sd,1),")"),
                            variable %in% median_vars ~ paste0(round(median,1)," (",round(q25,1),", ",round(q75,1),")"),
                            TRUE ~ paste0(round(freq,0)," (",round(proportion,1),"%)")
  )) %>% 
  dplyr::select(variable,group,study_sex,output) %>% 
  pivot_wider(names_from=study_sex,values_from=output) %>% 
  dplyr::select(variable,group,Total,Female_search,Male_search,Female_today,Male_today)

write_csv(table_df,"etiologic/paper/table_descriptive characteristics by study and sex.csv")  


#-----------------------------------------------------------------------------------------------------------
### STable 3 - sex*cluster

table_df <- read_csv("etiologic/analysis/dsy02_descriptive characteristics - total by sex and cluster.csv") %>% 
  dplyr::mutate(selected_rows = case_when(variable %in% mean_vars & est %in% c("mean","sd") ~ 1,
                                          variable %in% median_vars & est %in% c("median","q25","q75") ~ 1,
                                          !variable %in% c(mean_vars,median_vars) ~ 1,
                                          TRUE ~ 0
  )) %>% 
  dplyr::filter(selected_rows == 1) %>% 
  dplyr::select(sex_cluster,group,variable,est,value) %>% 
  pivot_wider(names_from=est,values_from=value) %>% 
  mutate(output = case_when(variable %in% mean_vars ~ paste0(round(mean,1)," (",round(sd,1),")"),
                            variable %in% median_vars ~ paste0(round(median,1)," (",round(q25,1),", ",round(q75,1),")"),
                            TRUE ~ paste0(round(freq,0)," (",round(proportion,1),"%)")
  )) %>% 
  dplyr::select(variable,group,sex_cluster,output) %>% 
  pivot_wider(names_from=sex_cluster,values_from=output) %>% 
  dplyr::select(variable,group,Total,Female_yMOD,Male_yMOD,Female_ySIDD,Male_ySIDD,Female_ySIRD,Male_ySIRD)

write_csv(table_df,"etiologic/paper/table_descriptive characteristics by sex and cluster.csv")  
