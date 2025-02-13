rm(list=ls());gc();source(".Rprofile")


etio_main <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/etiologic/dsy01_cross sectional df.RDS"))
# etio_cca <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/etiologic/complete cases/cca01_cross sectional complete cases df.RDS"))
etio_se <-  readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/etiologic/search only/se01_cross sectional df.RDS")) 

fact_main <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/factorial/dsy01_cross sectional df.RDS"))
# fact_cca <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/factorial/complete cases/cca01_cross sectional complete cases df.RDS"))
fact_se <-  readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/factorial/search only/se01_cross sectional df.RDS")) 

prov_main <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/prov/dsy01_cross sectional df.RDS"))
# prov_cca <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/prov/complete cases/cca01_cross sectional complete cases df.RDS"))
prov_se <-  readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/prov/search only/se01_cross sectional df.RDS")) 



calculate_summary <- function(data) {
  data %>%
    group_by(cluster) %>%
    summarise(
      bmi_mean = mean(bmi, na.rm = TRUE),
      bmi_sd = sd(bmi, na.rm = TRUE),
      hba1c_mean = mean(hba1c, na.rm = TRUE),
      hba1c_sd = sd(hba1c, na.rm = TRUE),
      cpeptidef_mean = mean(cpeptidef, na.rm = TRUE),
      cpeptidef_sd = sd(cpeptidef, na.rm = TRUE),
      sbp_mean = mean(sbp, na.rm = TRUE),
      sbp_sd = sd(sbp, na.rm = TRUE),
      dbp_mean = mean(dbp, na.rm = TRUE),
      dbp_sd = sd(dbp, na.rm = TRUE),
      ldlc_mean = mean(ldlc, na.rm = TRUE),
      ldlc_sd = sd(ldlc, na.rm = TRUE),
      hdlc_mean = mean(hdlc, na.rm = TRUE),
      hdlc_sd = sd(hdlc, na.rm = TRUE)
    )
}


etio_main_summary <- calculate_summary(etio_main)
# etio_cca_summary <- calculate_summary(etio_cca)
etio_se_summary <- calculate_summary(etio_se)

fact_main_summary <- calculate_summary(fact_main)
# fact_cca_summary <- calculate_summary(fact_cca)
fact_se_summary <- calculate_summary(fact_se)

prov_main_summary <- calculate_summary(prov_main)
# prov_cca_summary <- calculate_summary(prov_cca)
prov_se_summary <- calculate_summary(prov_se)


combined_summary <- bind_rows(
  etio_main_summary %>% mutate(subset = "etiologic", group = "main clusters"),
  # etio_cca_summary %>% mutate(subset = "etiologic", group = "complete cases"),
  etio_se_summary %>% mutate(subset = "etiologic", group = "search only"),
  fact_main_summary %>% mutate(subset = "factorial", group = "main clusters"),
  # fact_cca_summary %>% mutate(subset = "factorial", group = "complete cases"),
  fact_se_summary %>% mutate(subset = "factorial", group = "search only"),
  prov_main_summary %>% mutate(subset = "prov", group = "main clusters"),
  # prov_cca_summary %>% mutate(subset = "prov", group = "complete cases"),
  prov_se_summary %>% mutate(subset = "prov", group = "search only")
)


long_data <- combined_summary %>%
  pivot_longer(
    cols = matches("^(bmi|hba1c|cpeptidef|sbp|dbp|ldlc|hdlc)_"),  
    names_to = c(".value", "statistic"),  
    names_sep = "_" 
  ) %>%
  pivot_longer(
    cols = c(bmi, hba1c, cpeptidef, sbp, dbp, ldlc, hdlc), # Names of measurements
    names_to = "variable",
    values_to = "value"
  ) %>% 
  mutate(subset = str_replace_all(subset, "prov", "provider"))


fig <- ggplot(long_data, aes(x = cluster, y = value, shape = subset, color = group)) +
  geom_point() +
  facet_grid(variable ~ statistic, scales = "free_y", 
             labeller = labeller(statistic = c(mean = "Mean", sd = "SD"), 
                                 variable = c(bmi = "BMI", hba1c = "HbA1c", cpeptidef = "C-peptide",
                                              sbp = "SBP", dbp = "DBP", ldlc = "LDL", hdlc = "HDL"))) +
  theme_bw() +
  labs(
    x = "Subphenotype",
    y = "Cluster Variables",
    color = "Subset",
    shape = "Diagnosis Criteria"
  ) +
  theme(
    strip.text.x = element_text(face = "bold"),
    strip.text.y = element_text(face = "bold"),
    strip.background = element_rect(fill = "lightblue", colour = "deepskyblue4", size = 0.5, linetype = "solid")
  ) +
  scale_shape_manual(values = c(16, 17, 18, 19, 20, 21)) +  
  scale_color_manual(values = c("blue", "red")) 


ggsave(fig,filename=paste0(path_diabetes_subphenotypes_youth_folder,"/figures/mean and SD of cluster variables.tif"),width=12, height = 8)








