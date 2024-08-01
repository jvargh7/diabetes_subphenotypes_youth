rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

cross_weight <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/dsy03a_add weight cross-sectional.RDS")) %>% 
  dplyr::filter(!is.na(survey_score) & !is.na(exam_score))

proportions <- cross_weight %>%
  group_by(cluster) %>%
  summarise(
    prop_survey_abnormal = mean(survey_abnormal, na.rm = TRUE),
    prop_exam_abnormal = mean(exam_abnormal, na.rm = TRUE),
    prop_combined_abnormal = mean(combined_abnormal, na.rm = TRUE)
  ) %>% 
  pivot_longer(
    cols = -cluster,
    names_to = "abnormal_type",
    values_to = "proportion"
  )

proportions$cluster <- factor(proportions$cluster, levels = c("OB", "ID", "IR"))

fig_prop <- ggplot(proportions, aes(x = cluster, y = proportion, fill = abnormal_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("gold", "salmon", "deeppink")) +
  labs(y = "Proportion", x = "Cluster", title = "Proportion of Abnormal Scores by Cluster") +
  theme_minimal() +
  theme(legend.title = element_blank())

fig_prop %>% 
  ggsave(plot = ., filename = paste0(path_diabetes_subphenotypes_youth_folder,"/figures/proportion of abnormal scores by cluster.jpg"),width = 6, height = 4)
