rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

boxplot_df <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/dsy03a_add weight cross-sectional.RDS")) %>%
  dplyr::select("cluster","bmi", "hba1c", "cpeptidef", "sbp", "dbp", "ldlc", "hdlc") %>% 
  pivot_longer(
    cols = c("bmi", "hba1c", "cpeptidef", "sbp", "dbp", "ldlc", "hdlc"),
    names_to = "variable",
    values_to = "value"
  )
  
fig_boxplot <- ggplot(boxplot_df, aes(x = cluster, y = value, fill = cluster)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 16, outlier.size = 1) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = "Measurement", title = "Distribution of clinical characteristics by cluster") +
  scale_fill_brewer(palette = "Set3") 

fig_boxplot %>% 
  ggsave(plot = ., filename = paste0(path_diabetes_subphenotypes_youth_folder,"/figures/distribution of clinicalcharacteristics by cluster.jpg"),width = 6, height = 6)
