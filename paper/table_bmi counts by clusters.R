rm(list=ls());gc();source(".Rprofile")

crosssec_df <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/dsy01a_cross sectional df.RDS")) 

bmi_thresholds <- quantile(crosssec_df$bmi_residual, probs = c(1/3, 2/3))

crosssec_df$bmi_category <- cut(crosssec_df$bmi_residual, 
                               breaks = c(-Inf, bmi_thresholds[1], bmi_thresholds[2], Inf), 
                               labels = c("Low BMI", 
                                          "Mod BMI", 
                                          "High BMI"),
                               include.lowest = TRUE)

result_table <- crosssec_df %>%
  group_by(bmi_category, cluster) %>%
  summarise(unique_study_ids = n_distinct(study_id), .groups = 'drop')

# Pivot the data to create the required table format
pivot_table <- tidyr::pivot_wider(result_table, 
                                  names_from = cluster, 
                                  values_from = unique_study_ids,
                                  values_fill = list(unique_study_ids = 0)) # fill NA with 0