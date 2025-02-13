rm(list=ls());gc();source(".Rprofile")

boxplot_df <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/etiologic/dsy01_cross sectional df.RDS")) %>% 
  dplyr::select("study_id","cluster", "age_category",
                "bmi_residual","hba1c_residual","cpeptidef_residual", "sbp_residual","dbp_residual","ldlc_residual","hdlc_residual")


cluster_colors = c("yOD"="#F8BDA4","yIRD"="#6B8E6A","yIDD"="#ACD9EA")

age_data <- data.frame(
  cluster = c('yOD', 'yIDD', 'yIRD', 'yOD', 'yIDD', 'yIRD', 'yOD', 'yIDD', 'yIRD'),
  age_category = c('≤13y', '≤13y', '≤13y', '14-15y', '14-15y', '14-15y', '>15y', '>15y', '>15y'),
  percentage = c(38.6, 40, 36.7, 23.2, 23.3, 21.9, 38.3, 36.7, 41.4)
)

age_data$age_category <- factor(age_data$age_category, levels = c("≤13y", "14-15y", ">15y"))

age_colors = c("≤13y"="#F8BDA4","14-15y"="#6B8E6A",">15y"="#ACD9EA")

fig_A = age_data %>%
  ggplot(data=., aes(x=age_category, y=percentage, fill=cluster)) +
  geom_col(position = position_dodge(width=0.9),color = "black") +
  xlab("") +
  ylab("Age Category (%)") +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors)

fig_B = boxplot_df %>% 
  ggplot(data=.,aes(x=cluster,y=hba1c_residual,fill=cluster)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab("HbA1c Residuals (%)") +
  scale_y_continuous(limits=c(0,10),breaks=seq(0,10,by=2)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors)

fig_C = boxplot_df %>% 
  ggplot(data=.,aes(x=cluster,y=bmi_residual,fill=cluster)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab(bquote('BMI Residuals ( kg' /m^2~')')) +
  scale_y_continuous(limits=c(0,60),breaks=seq(0,60,by=10)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors)

fig_D = boxplot_df %>% 
  ggplot(data=.,aes(x=cluster,y=cpeptidef_residual,fill=cluster)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab("Fasting C-peptide Residuals (ng/mL)") +
  scale_y_continuous(limits=c(0,10),breaks=seq(0,10,by=2)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors)

fig_E = boxplot_df %>% 
  ggplot(data=.,aes(x=cluster,y=ldlc_residual,fill=cluster)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab("LDL Cholesterol Residuals (mg/dL)") +
  scale_y_continuous(limits=c(0,150),breaks=seq(0,150,by=50)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors)

fig_F = boxplot_df %>% 
  ggplot(data=.,aes(x=cluster,y=hdlc_residual,fill=cluster)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab("HDL Cholesterol Residuals (mg/dL)") +
  scale_y_continuous(limits=c(0,40),breaks=seq(0,40,by=10)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors)

fig_G = boxplot_df %>% 
  ggplot(data=.,aes(x=cluster,y=sbp_residual,fill=cluster)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab("SBP Residuals (mmHg)") +
  scale_y_continuous(limits=c(0,50),breaks=seq(0,50,by=10)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors)

fig_H = boxplot_df %>% 
  ggplot(data=.,aes(x=cluster,y=dbp_residual,fill=cluster)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab("DBP Residuals (mmHg)") +
  scale_y_continuous(limits=c(0,40),breaks=seq(0,40,by=10)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors)



library(ggpubr)

ggarrange(fig_A,
          fig_B,
          fig_C,
          fig_D,
          fig_E,
          fig_F,
          fig_G,
          fig_H,
          nrow=2,
          ncol=4,
          common.legend = TRUE,legend = "none") %>% 
  ggsave(.,filename=paste0(path_diabetes_subphenotypes_youth_folder,"/figures/etiologic/distribution of clinical characteristics residuals by cluster.tif"),width=12,height =6)
