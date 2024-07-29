rm(list=ls());gc();source(".Rprofile")

### Elbow Plot
library(ggplot2)

imputed_data <- read.csv(paste0(path_diabetes_subphenotypes_youth_folder, '/working/cleaned/setdy02_kmeans imputation.csv')) %>% 
  dplyr::select(bmi, hba1c, cpeptidef, sbp, dbp, ldlc, hdlc)

# Scaling the data can often yield better clustering results
data_scaled <- scale(imputed_data)

# Function to compute total within-cluster sum of square
wss <- function(k) {
  kmeans(data_scaled, centers=k, nstart=10)$tot.withinss
}

# Compute and plot WSS for different numbers of clusters
k_values <- 1:11  
wss_values <- sapply(k_values, wss)

elbow_plot <- ggplot(data.frame(k = k_values, wss = wss_values), aes(x = k, y = wss)) +
  geom_point(size = 3, color = "blue") +  # Increase point size and change color
  geom_line(color = "blue") +  # Change line color to match points
  scale_x_continuous(breaks = 1:11) +  # Explicit x-axis breaks for each integer from 1 to 11
  labs(title = "Elbow Plot for Optimal Number of Clusters",
       x = "Number of clusters (k)",
       y = "Total within-cluster sum of squares")
  theme_minimal() +  # Use a minimal theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold the title
    axis.title = element_text(face = "bold"),  # Bold axis titles
    axis.text = element_text(color = "black"),  # Black color for axis texts
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white", colour = "white")
  )

ggsave(elbow_plot,filename=paste0(path_diabetes_subphenotypes_youth_folder,"/figures/elbow plot for optimal number of clusters.png"),width=6, height = 4)



## STable 1 - included | excluded

source_df <- read.csv(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/setdy01c_source sample.csv"))
analytic_dataset_cluster <- read.csv(paste0(path_diabetes_subphenotypes_youth_folder, '/working/cleaned/setdy03_kmeans clustering.csv'))

source("functions/table1_summary.R")

names(analytic_dataset_cluster)

c_vars = c("bmi","hba1c","cpeptidef", "sbp","dbp","ldlc","hdlc","totalc","insulinf","tgl","glucosef")
p_vars = c("female","insulin","metformin")
g_vars = c("study","dmduration_category","age_category","race_eth")

table_df = source_df %>% 
  mutate(study_cca = case_when(
    study == "SEARCH" & (study_id %in% analytic_dataset_cluster$study_id) ~ "search_included",
    study == "SEARCH" & !(study_id %in% analytic_dataset_cluster$study_id) ~ "search_excluded",
    study == "TODAY" & (study_id %in% analytic_dataset_cluster$study_id) ~ "today_included",
    study == "TODAY" & !(study_id %in% analytic_dataset_cluster$study_id) ~ "today_excluded"
  )) %>% 
  bind_rows(.,
            {.} %>% 
              mutate(study_cca="Total")) %>% 
  table1_summary(.,c_vars = c_vars,p_vars = p_vars,g_vars = g_vars,id_vars = "study_cca")

write_csv(table_df,"analysis/dsy01a_descriptive characteristics - total by study and cca.csv")



### Table 1 by clusters

mean_vars = c("bmi","cpeptidef", "sbp","dbp","ldlc","hdlc","totalc","insulinf","tgl","glucosef")

median_vars = c("hba1c")

table_df <- read_csv("analysis/dsy01a_descriptive characteristics - total by study and cca.csv") %>% 
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

write_csv(table_df,"paper/table_descriptive characteristics by study and cca.csv")  



### STable 2 - study*sex
analytic_dataset_cluster <- read.csv(paste0(path_diabetes_subphenotypes_youth_folder, '/working/cleaned/setdy03_kmeans clustering.csv'))

source("functions/table1_summary.R")

names(analytic_dataset_cluster)

c_vars = c("bmi","hba1c","cpeptidef", "sbp","dbp","ldlc","hdlc","totalc","insulinf","tgl","glucosef")
p_vars = c("female","insulin","metformin")
g_vars = c("cluster","study","dmduration_category","age_category","race_eth")

table_df = analytic_dataset_cluster %>% 
  mutate(study_sex = case_when(female == 1 & study == "SEARCH" ~ "Female_search",
                               female == 0 & study == "SEARCH" ~ "Male_search",
                               female == 1 & study == "TODAY" ~ "Female_today",
                               female == 0 & study == "TODAY" ~ "Male_today")) %>% 
  bind_rows(.,
            {.} %>% 
              mutate(study_sex="Total")) %>% 
  table1_summary(.,c_vars = c_vars,p_vars = p_vars,g_vars = g_vars,id_vars = "study_sex")

write_csv(table_df,"analysis/dsy01b_descriptive characteristics - total by study and sex.csv")


### Table 1 by clusters

mean_vars = c("bmi","cpeptidef", "sbp","dbp","ldlc","hdlc","totalc","insulinf","tgl","glucosef")

median_vars = c("hba1c")

table_df <- read_csv("analysis/dsy01b_descriptive characteristics - total by study and sex.csv") %>% 
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

write_csv(table_df,"paper/table_descriptive characteristics by study and sex.csv")  




### STable 3 - sex*cluster

table_df = analytic_dataset_cluster %>% 
  mutate(sex_cluster = case_when(female == 1 & cluster == "OB" ~ "Female_OB",
                               female == 0 & cluster == "OB" ~ "Male_OB",
                               female == 1 & cluster == "ID" ~ "Female_ID",
                               female == 0 & cluster == "ID" ~ "Male_ID",
                               female == 1 & cluster == "IR" ~ "Female_IR",
                               female == 0 & cluster == "IR" ~ "Male_IR")) %>% 
  bind_rows(.,
            {.} %>% 
              mutate(sex_cluster="Total")) %>% 
  table1_summary(.,c_vars = c_vars,p_vars = p_vars,g_vars = g_vars,id_vars = "sex_cluster")

write_csv(table_df,"analysis/dsy01b_descriptive characteristics - total by sex and cluster.csv")


### Table 1 by clusters

mean_vars = c("bmi","cpeptidef", "sbp","dbp","ldlc","hdlc","totalc","insulinf","tgl","glucosef")

median_vars = c("hba1c")

table_df <- read_csv("analysis/dsy01b_descriptive characteristics - total by sex and cluster.csv") %>% 
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
  dplyr::select(variable,group,Total,Female_OB,Male_OB,Female_ID,Male_ID,Female_IR,Male_IR)

write_csv(table_df,"paper/table_descriptive characteristics by sex and cluster.csv")  

