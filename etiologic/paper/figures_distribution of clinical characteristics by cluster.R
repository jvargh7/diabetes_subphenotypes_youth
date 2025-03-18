rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

boxplot_df <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/etiologic/dsy01_cross sectional df.RDS")) %>%
  dplyr::select("study_id","cluster", "hba1c", "cpeptidef", "sbp", "dbp", "ldlc", "hdlc", "bmi","age_category")

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
  ggplot(data=.,aes(x=cluster,y=hba1c,fill=cluster)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab("HbA1c (%)") +
  scale_y_continuous(limits=c(0,20),breaks=seq(0,20,by=5)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors)

fig_C = boxplot_df %>% 
  ggplot(data=.,aes(x=cluster,y=bmi,fill=cluster)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab(bquote('BMI ( kg' /m^2~')')) +
  scale_y_continuous(limits=c(0,60),breaks=seq(0,60,by=10)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors)

fig_D = boxplot_df %>% 
  ggplot(data=.,aes(x=cluster,y=cpeptidef,fill=cluster)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab("Fasting C-peptide (ng/mL)") +
  scale_y_continuous(limits=c(0,15),breaks=seq(0,15,by=5)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors)

fig_E = boxplot_df %>% 
  ggplot(data=.,aes(x=cluster,y=ldlc,fill=cluster)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab("LDL Cholesterol (mg/dL)") +
  scale_y_continuous(limits=c(0,250),breaks=seq(0,250,by=50)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors)

fig_F = boxplot_df %>% 
  ggplot(data=.,aes(x=cluster,y=hdlc,fill=cluster)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab("HDL Cholesterol (mg/dL)") +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,by=20)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors)

fig_G = boxplot_df %>% 
  ggplot(data=.,aes(x=cluster,y=sbp,fill=cluster)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab("SBP (mmHg)") +
  scale_y_continuous(limits=c(0,200),breaks=seq(0,200,by=50)) +
  theme_bw() +
  scale_fill_manual(name="",values=cluster_colors)

fig_H = boxplot_df %>% 
  ggplot(data=.,aes(x=cluster,y=dbp,fill=cluster)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  xlab("") +
  ylab("DBP (mmHg)") +
  scale_y_continuous(limits=c(0,120),breaks=seq(0,120,by=20)) +
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
          common.legend = TRUE,legend = "bottom") %>% 
  ggsave(.,filename=paste0(path_diabetes_subphenotypes_youth_folder,"/figures/etiologic/distribution of clinical characteristics by cluster.tif"),width=12,height =5.5)
