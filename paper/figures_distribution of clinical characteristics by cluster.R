rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

boxplot_df <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/dsy01a_cross sectional df.RDS")) %>%
  dplyr::select("cluster", "hba1c", "cpeptidef", "sbp", "dbp", "ldlc", "hdlc", "bmi") %>% 
  pivot_longer(
    cols = c("bmi", "hba1c", "cpeptidef", "sbp", "dbp", "ldlc", "hdlc"),
    names_to = "variable",
    values_to = "value"
  ) %>% 
  mutate(variable = factor(variable, levels = c("bmi", "cpeptidef", "dbp", "sbp", "hdlc", "ldlc","hba1c")))

variable_labels <- c(
  cpeptidef = "C-peptide [ng/mL]",
  hba1c = "HbA1c [%]",
  hdlc = "HDL Cholesterol [mg/dL]",
  ldlc = "LDL Cholesterol [mg/dL]",
  sbp = "SBP [mmHg]",
  dbp = "DBP [mmHg]",
  bmi = "BMI [kg/m²]"
)

fig_boxplot <- ggplot(boxplot_df, aes(x = cluster, y = value, fill = cluster)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 16, outlier.size = 1) +
  facet_wrap(~variable, scales = "free", labeller = labeller(variable = variable_labels), nrow = 2, ncol = 4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "bottom") +
  labs(x = NULL, y = NULL, title = "Distribution of Clinical Characteristics by Subphenotypes", fill = "Subphenotype") +
  scale_fill_brewer(palette = "Set3")

fig_boxplot %>% 
  ggsave(plot = ., filename = paste0(path_diabetes_subphenotypes_youth_folder,"/figures/distribution of clinicalcharacteristics by cluster.jpg"),width = 8, height = 5.5)

