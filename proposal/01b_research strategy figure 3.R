
set.seed(2022)

today_mnsi <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/today/today_mnsi.RDS")) %>% 
  left_join(readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/today/today_baseline.RDS")) %>% 
              mutate(age_imp = case_when(age == "<=13" ~ runif(nrow(.),min=10,max=13),
                                         age == "14-15" ~ rbinom(nrow(.),1,p=0.5) + 14,
                                         age == ">15" ~ rbinom(nrow(.),1,p=0.5) + 16),
                    treatment = factor(treatment,levels=c(1:3),labels=c("Metformin only","Metformin + Rosiglitazone","Metformin + Lifestyle"))
              
              ) %>% 
              dplyr::select(study_id,age_imp,treatment,dmduration)%>% 
              rename(dmduration_baseline = dmduration) ,
            by = c("study_id"))    %>% 
  mutate(age = age_imp + randdays/365)  %>% 
  dplyr::filter(dmduration_baseline == "<=5 months")

search_mnsi <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/search/search_mnsi.RDS"))  %>% 
  left_join(readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/search/search_baseline.RDS")) %>% 
              dplyr::select(study_id,dmduration) %>% 
              rename(dmduration_baseline = dmduration),
            by = "study_id") %>% 
  mutate(study_id = as.character(study_id)) %>% 
  dplyr::filter(dmduration_baseline >= 0 & dmduration_baseline <= duration_cutoff)


fig3_df <- bind_rows(today_mnsi %>% mutate(study = "TODAY") %>% 
                    dplyr::select(study_id,study,age,obsmnsir,obsmnsil,treatment),
                  search_mnsi %>% mutate(study = "SEARCH") %>% 
                    dplyr::select(study_id,study,age,obsmnsir,obsmnsil) %>% 
                    mutate(treatment = "SEARCH")
                  ) %>% 
  mutate_at(vars(obsmnsir,obsmnsil),function(x) case_when(x=="Abnormal" ~ 1,
                                                          x == "Normal" ~ 0,
                                                          TRUE ~ NA_real_)) 

fig3A <- fig3_df %>%  
  ggplot(data=.,aes(x=age,y=obsmnsil,color=treatment,fill=treatment,group=treatment)) +
  geom_smooth() +
  theme_bw() +
  xlab("Age (years)") +
  ylab("Probability of abnormal foot") +
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,by=0.2)) +
  scale_color_discrete("") +
  scale_fill_discrete("") +
  # https://stackoverflow.com/questions/10836843/ggplot2-plot-area-margins
  theme(plot.margin = unit(c(1,0.1,0.1,0.1),"cm"))

fig3B <- fig3_df %>%  
  ggplot(data=.,aes(x=age,y=obsmnsir,color=treatment,fill=treatment,group=treatment)) +
  geom_smooth() +
  theme_bw() +
  xlab("Age (years)") +
  ylab("Probability of abnormal foot") +
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,by=0.2)) +
  scale_color_discrete("") +
  scale_fill_discrete("") +
  theme(plot.margin = unit(c(1,0.1,0.1,0.1),"cm"))


require(ggpubr)
ggarrange(fig3A,fig3B,
          nrow=1,ncol=2,
          labels=c("A. Left","B. Right"),
          common.legend=TRUE,
          legend="bottom") %>% 
  ggsave(.,filename = paste0(path_diabetes_subphenotypes_youth_folder,"/figures/research strategy figure_probability abnormal foot.png"),width=8,height=4)

fig3_df %>% 
  distinct(study_id,.keep_all=TRUE) %>% 
  group_by(study) %>% 
  tally()
