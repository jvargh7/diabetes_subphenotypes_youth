rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

crosssec_df <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/dsy01a_cross sectional df.RDS")) %>% 
  dplyr::filter(!is.na(survey_score) & !is.na(exam_score))

proportions <- crosssec_df %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    prop_survey_abnormal = mean(survey_abnormal, na.rm = TRUE),
    prop_exam_abnormal = mean(exam_abnormal, na.rm = TRUE),
    prop_combined_abnormal = mean(combined_abnormal, na.rm = TRUE)
  ) %>% 
  pivot_longer(
    cols = -c(cluster, n),
    names_to = "abnormal_type",
    values_to = "proportion"
  ) %>% 
  mutate(se = sqrt(proportion * (1 - proportion) / n)) %>% 
  mutate(lci = proportion - 1.96 * se,
         uci = proportion + 1.96 * se) %>% 
  mutate(across(c("proportion", "se", "lci", "uci"), ~ . * 100))

proportions$cluster <- factor(proportions$cluster, levels = c("MOD", "SIDD", "SIRD"))


fig_prop <- ggplot(proportions, aes(x = cluster, y = proportion, fill = abnormal_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = lci, ymax = uci), 
                position = position_dodge(width = 0.7), 
                width = 0.25) +
  scale_fill_manual(values = c("gold", "salmon", "deeppink"), 
                    labels = c("Abnormal Combined Score", "Abnormal Examination Score", "Abnormal Questionnaire Score")) +
  labs(y = "Proportion", x = "Subphenotype", 
       title = "Proportion of Abnormal Scores by Subphenotype", 
       fill = "Abnormal Score Type") +
  theme_minimal() +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"))


fig_prop %>% 
  ggsave(plot = ., filename = paste0(path_diabetes_subphenotypes_youth_folder,"/figures/proportion of abnormal scores by cluster.jpg"),width = 6, height = 4)
