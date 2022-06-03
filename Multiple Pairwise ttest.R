rm(list=ls(all=TRUE))
x=readline()
F:\E Drive\MyFiles\MyFiles\PhD Research\Illusion Draft\Alireza_data
setwd(x)
getwd()



library(pacman)
p_load(reshape2,
       ez,
       lme4,
       lmerTest,
       ggplot2,
       grid,
       tidyr,
       Rmisc,
       dplyr,
       effects,
       gridExtra,
       psych,
       Cairo,
       corrplot,
       knitr,
       PerformanceAnalytics,
       officer,
       rvg,
       tidyverse,
       ggpubr,
       rstatix,
       emmeans)


graph_defaults=theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_text(size = 8,vjust=-0.25))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.text.x = element_text(size = 8)) + 
  theme(axis.text.y = element_text(size = 8))+
  theme(plot.title = element_text(size = 8, face="bold"))+
  theme(legend.text=element_text(size = 8))+
  #theme(legend.title = element_text(colour="black", size = 8, face="bold"))+
  #theme(legend.position="right",legend.title=element_blank(),legend.key.height=unit(.5,"line"))+
  theme(strip.text.x = element_text(size = 8, face="bold"),
        strip.text.y = element_text(size = 8, face="bold"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.background = element_rect(fill = "transparent",colour = NA))+
  theme(plot.background = element_rect(fill = "transparent",colour = NA))+
  theme(strip.background = element_rect(fill="transparent"))
  # scale_fill_grey()+
  # theme(panel.grid = element_blank(),
  #       panel.border = element_blank())


# Read Data  ---------------------------------------------------------

DesiredVariables = c("subj","age2","group","R_corr_rate", "R_incorr_rate",
                     "Treas_corr_rate", "Treas_incorr_rate",
                     "Treas_R_match", "Treas_F_match",
                     "Treas_R_nonmatch", "Treas_F_nonmatch",
                     "Gender",
                     "match_pc", "nonmatch_pc","BetOrder",
                     "Treas_match_rate", "Treas_nonmatch_rate",
                     "R_match_rate", "R_nonmatch_rate",
                     "Treas_R", "Treas_F",
                     "R_match_corr_rate", "R_match_incorr_rate", "R_nonmatch_corr_rate", "R_nonmatch_incorr_rate",
                     "Treas_match_corr_rate", "Treas_match_incorr_rate", "Treas_nonmatch_corr_rate", "Treas_nonmatch_incorr_rate")

d=read.table('all_data_subjremoved.xls', header=TRUE, sep="\t", strip.white = TRUE)
d$age2=factor(d$age2, levels = c("YC", "OC", "adults"),labels = c("6_7_years", "9_10_years", "Adults"))
d$group = unique("exp1");
dexp1 = d[,DesiredVariables]


d=read.table('alldata_relevant_subjremoved.xls', header=TRUE, sep="\t", strip.white = TRUE)
d$age2=factor(d$age2, levels = c("child", "adults"), labels = c("6_7_years", "Adults"))
d$group = case_when(d$Group==2~"exp2",
                    d$Group==1~"exp3")

dexp2 = d[,DesiredVariables]


d = rbind(dexp1,dexp2)




############################### First Table: Remember Rates ----
Results = NULL

data=subset(d, select=c(subj, R_match_corr_rate, R_match_incorr_rate, R_nonmatch_corr_rate, R_nonmatch_incorr_rate, age2,group))

as.data.frame(summarise(group_by(data,group,age2),DFmean__Match = round(mean(R_match_corr_rate,na.rm=T)-mean(R_match_incorr_rate,na.rm=T),2),
                                      DFmean__nonMatch = round(mean(R_nonmatch_corr_rate,na.rm=T)-mean(R_nonmatch_incorr_rate,na.rm=T),2)))

Meltdat=melt(data, id.vars = c("age2","subj","group"), 
             variable.name = "condition")

Meltdat$Conds = paste(Meltdat$group,Meltdat$age2,Meltdat$condition,sep = "_")

pwc <- Meltdat  %>%
  pairwise_t_test(value ~ Conds, pool.sd = FALSE,var.equal=T, paired = T,
                  comparisons = list(c("exp1_6_7_years_R_match_corr_rate","exp1_6_7_years_R_match_incorr_rate"),
                                     c("exp1_9_10_years_R_match_corr_rate","exp1_9_10_years_R_match_incorr_rate"),
                                     c("exp1_Adults_R_match_corr_rate","exp1_Adults_R_match_incorr_rate"),
                                     c("exp1_6_7_years_R_nonmatch_corr_rate","exp1_6_7_years_R_nonmatch_incorr_rate"),
                                     c("exp1_9_10_years_R_nonmatch_corr_rate","exp1_9_10_years_R_nonmatch_incorr_rate"),
                                     c("exp1_Adults_R_nonmatch_corr_rate","exp1_Adults_R_nonmatch_incorr_rate")),
                  p.adjust.method = "bonferroni")
pwc$p = round(pwc$p,3)
pwc$p.adj = round(pwc$p.adj,3)
Ps = as.data.frame(pwc)
Ps$exp = unique("exp1")

CD =as.data.frame(cohens_d(Meltdat,value~Conds, var.equal = T,paired = T,
                           comparisons = list(c("exp1_6_7_years_R_match_corr_rate","exp1_6_7_years_R_match_incorr_rate"),
                                              c("exp1_9_10_years_R_match_corr_rate","exp1_9_10_years_R_match_incorr_rate"),
                                              c("exp1_Adults_R_match_corr_rate","exp1_Adults_R_match_incorr_rate"),
                                              c("exp1_6_7_years_R_nonmatch_corr_rate","exp1_6_7_years_R_nonmatch_incorr_rate"),
                                              c("exp1_9_10_years_R_nonmatch_corr_rate","exp1_9_10_years_R_nonmatch_incorr_rate"),
                                              c("exp1_Adults_R_nonmatch_corr_rate","exp1_Adults_R_nonmatch_incorr_rate")) ))
CD$effsize = round(CD$effsize,2)
Ps$effectsize = CD$effsize
Results = rbind(Results,Ps)

pwc <- Meltdat  %>%
  pairwise_t_test(value ~ Conds, pool.sd = FALSE,var.equal=T, paired = T,
                  comparisons = list(c("exp2_6_7_years_R_match_corr_rate","exp2_6_7_years_R_match_incorr_rate"),
                                     c("exp2_Adults_R_match_corr_rate","exp2_Adults_R_match_incorr_rate"),
                                     c("exp2_6_7_years_R_nonmatch_corr_rate","exp2_6_7_years_R_nonmatch_incorr_rate"),
                                     c("exp2_Adults_R_nonmatch_corr_rate","exp2_Adults_R_nonmatch_incorr_rate")),
                  p.adjust.method = "bonferroni")
pwc$p = round(pwc$p,3)
pwc$p.adj = round(pwc$p.adj,3)
Ps = as.data.frame(pwc)
Ps$exp = unique("exp2")
CD =as.data.frame(cohens_d(Meltdat,value~Conds, var.equal = T,paired = T,
                           comparisons = list(c("exp2_6_7_years_R_match_corr_rate","exp2_6_7_years_R_match_incorr_rate"),
                                              c("exp2_Adults_R_match_corr_rate","exp2_Adults_R_match_incorr_rate"),
                                              c("exp2_6_7_years_R_nonmatch_corr_rate","exp2_6_7_years_R_nonmatch_incorr_rate"),
                                              c("exp2_Adults_R_nonmatch_corr_rate","exp2_Adults_R_nonmatch_incorr_rate")) ))
CD$effsize = round(CD$effsize,2)
Ps$effectsize = CD$effsize
Results = rbind(Results,Ps)

pwc <- Meltdat  %>%
  pairwise_t_test(value ~ Conds, pool.sd = FALSE,var.equal=T, paired = T,
                  comparisons = list(c("exp3_6_7_years_R_match_corr_rate","exp3_6_7_years_R_match_incorr_rate"),
                                     c("exp3_Adults_R_match_corr_rate","exp3_Adults_R_match_incorr_rate"),
                                     c("exp3_6_7_years_R_nonmatch_corr_rate","exp3_6_7_years_R_nonmatch_incorr_rate"),
                                     c("exp3_Adults_R_nonmatch_corr_rate","exp3_Adults_R_nonmatch_incorr_rate")),
                  p.adjust.method = "bonferroni")
pwc$p = round(pwc$p,3)
pwc$p.adj = round(pwc$p.adj,3)
Ps = as.data.frame(pwc)
Ps$exp = unique("exp3")
CD =as.data.frame(cohens_d(Meltdat,value~Conds, var.equal = T,paired = T,
                           comparisons = list(c("exp3_6_7_years_R_match_corr_rate","exp3_6_7_years_R_match_incorr_rate"),
                                              c("exp3_Adults_R_match_corr_rate","exp3_Adults_R_match_incorr_rate"),
                                              c("exp3_6_7_years_R_nonmatch_corr_rate","exp3_6_7_years_R_nonmatch_incorr_rate"),
                                              c("exp3_Adults_R_nonmatch_corr_rate","exp3_Adults_R_nonmatch_incorr_rate")) ))
CD$effsize = round(CD$effsize,2)
Ps$effectsize = CD$effsize
Results = rbind(Results,Ps)

write.csv(Results,"Stats_Remember.csv")



############################### First Table: Treasure Rates ----
Results = NULL

data=subset(d, select=c(subj, Treas_match_corr_rate, Treas_match_incorr_rate, Treas_nonmatch_corr_rate, Treas_nonmatch_incorr_rate, age2,group))

as.data.frame(summarise(group_by(data,group,age2),DFmean__Match = round(mean(Treas_match_corr_rate,na.rm=T)-mean(Treas_match_incorr_rate,na.rm=T),2),
                        DFmean__nonMatch = round(mean(Treas_nonmatch_corr_rate,na.rm=T)-mean(Treas_nonmatch_incorr_rate,na.rm=T),2)))

Meltdat=melt(data, id.vars = c("age2","subj","group"), 
             variable.name = "condition")

Meltdat$Conds = paste(Meltdat$group,Meltdat$age2,Meltdat$condition,sep = "_")

pwc <- Meltdat  %>%
  pairwise_t_test(value ~ Conds, pool.sd = FALSE,var.equal=T, paired = T,
                  comparisons = list(c("exp1_6_7_years_Treas_match_corr_rate","exp1_6_7_years_Treas_match_incorr_rate"),
                                     c("exp1_9_10_years_Treas_match_corr_rate","exp1_9_10_years_Treas_match_incorr_rate"),
                                     c("exp1_Adults_Treas_match_corr_rate","exp1_Adults_Treas_match_incorr_rate"),
                                     c("exp1_6_7_years_Treas_nonmatch_corr_rate","exp1_6_7_years_Treas_nonmatch_incorr_rate"),
                                     c("exp1_9_10_years_Treas_nonmatch_corr_rate","exp1_9_10_years_Treas_nonmatch_incorr_rate"),
                                     c("exp1_Adults_Treas_nonmatch_corr_rate","exp1_Adults_Treas_nonmatch_incorr_rate")),
                  p.adjust.method = "bonferroni")
pwc$p = round(pwc$p,3)
pwc$p.adj = round(pwc$p.adj,3)
Ps = as.data.frame(pwc)
Ps$exp = unique("exp1")
CD =as.data.frame(cohens_d(Meltdat,value~Conds, var.equal = T,paired = T,
                           comparisons = list(c("exp1_6_7_years_Treas_match_corr_rate","exp1_6_7_years_Treas_match_incorr_rate"),
                                              c("exp1_9_10_years_Treas_match_corr_rate","exp1_9_10_years_Treas_match_incorr_rate"),
                                              c("exp1_Adults_Treas_match_corr_rate","exp1_Adults_Treas_match_incorr_rate"),
                                              c("exp1_6_7_years_Treas_nonmatch_corr_rate","exp1_6_7_years_Treas_nonmatch_incorr_rate"),
                                              c("exp1_9_10_years_Treas_nonmatch_corr_rate","exp1_9_10_years_Treas_nonmatch_incorr_rate"),
                                              c("exp1_Adults_Treas_nonmatch_corr_rate","exp1_Adults_Treas_nonmatch_incorr_rate")) ))
CD$effsize = round(CD$effsize,2)
Ps$effectsize = CD$effsize
Results = rbind(Results,Ps)

pwc <- Meltdat  %>%
  pairwise_t_test(value ~ Conds, pool.sd = FALSE,var.equal=T, paired = T,
                  comparisons = list(c("exp2_6_7_years_Treas_match_corr_rate","exp2_6_7_years_Treas_match_incorr_rate"),
                                     c("exp2_Adults_Treas_match_corr_rate","exp2_Adults_Treas_match_incorr_rate"),
                                     c("exp2_6_7_years_Treas_nonmatch_corr_rate","exp2_6_7_years_Treas_nonmatch_incorr_rate"),
                                     c("exp2_Adults_Treas_nonmatch_corr_rate","exp2_Adults_Treas_nonmatch_incorr_rate")),
                  p.adjust.method = "bonferroni")
pwc$p = round(pwc$p,3)
pwc$p.adj = round(pwc$p.adj,3)
Ps = as.data.frame(pwc)
Ps$exp = unique("exp2")
CD =as.data.frame(cohens_d(Meltdat,value~Conds, var.equal = T,paired = T,
                           comparisons = list(c("exp2_6_7_years_Treas_match_corr_rate","exp2_6_7_years_Treas_match_incorr_rate"),
                                              c("exp2_Adults_Treas_match_corr_rate","exp2_Adults_Treas_match_incorr_rate"),
                                              c("exp2_6_7_years_Treas_nonmatch_corr_rate","exp2_6_7_years_Treas_nonmatch_incorr_rate"),
                                              c("exp2_Adults_Treas_nonmatch_corr_rate","exp2_Adults_Treas_nonmatch_incorr_rate")) ))
CD$effsize = round(CD$effsize,2)
Ps$effectsize = CD$effsize
Results = rbind(Results,Ps)

pwc <- Meltdat  %>%
  pairwise_t_test(value ~ Conds, pool.sd = FALSE,var.equal=T, paired = T,
                  comparisons = list(c("exp3_6_7_years_Treas_match_corr_rate","exp3_6_7_years_Treas_match_incorr_rate"),
                                     c("exp3_Adults_Treas_match_corr_rate","exp3_Adults_Treas_match_incorr_rate"),
                                     c("exp3_6_7_years_Treas_nonmatch_corr_rate","exp3_6_7_years_Treas_nonmatch_incorr_rate"),
                                     c("exp3_Adults_Treas_nonmatch_corr_rate","exp3_Adults_Treas_nonmatch_incorr_rate")),
                  p.adjust.method = "bonferroni")
pwc$p = round(pwc$p,3)
pwc$p.adj = round(pwc$p.adj,3)
Ps = as.data.frame(pwc)
Ps$exp = unique("exp3")
CD =as.data.frame(cohens_d(Meltdat,value~Conds, var.equal = T,paired = T,
                           comparisons = list(c("exp3_6_7_years_Treas_match_corr_rate","exp3_6_7_years_Treas_match_incorr_rate"),
                                              c("exp3_Adults_Treas_match_corr_rate","exp3_Adults_Treas_match_incorr_rate"),
                                              c("exp3_6_7_years_Treas_nonmatch_corr_rate","exp3_6_7_years_Treas_nonmatch_incorr_rate"),
                                              c("exp3_Adults_Treas_nonmatch_corr_rate","exp3_Adults_Treas_nonmatch_incorr_rate")) ))
CD$effsize = round(CD$effsize,2)
Ps$effectsize = CD$effsize
Results = rbind(Results,Ps)

write.csv(Results,"Stats_Treasure.csv")
############################### First Table: Remember Rates old----
Results = NULL
A = t.test(d$R_corr_rate[d$group=="exp1"&d$age2 == "6_7_years"],d$R_incorr_rate[d$group=="exp1"&d$age2 == "6_7_years"],paired = T)
Meltdat=melt(d[d$group == "exp1",], id.vars = c("age2","subj"), 
                measure.vars = c("R_corr_rate", "R_incorr_rate"), 
                variable.name = "condition")
Meltdat$age2 = as.factor(Meltdat$age2) 
Meltdat$condition = factor(Meltdat$condition, 
                           levels = c("R_corr_rate", "R_incorr_rate"),labels = c("Remember_Cor","Remember_Inc"))


Meltdat$Conds = paste(Meltdat$group,Meltdat$age2,Meltdat$condition,sep = " ")
pwc <- Meltdat  %>%
  pairwise_t_test(value ~ Conds, pool.sd = FALSE,var.equal=T,
    p.adjust.method = "bonferroni")
Ps = pwc
Ps$exp = unique("EXp1")
summarise(group_by(Meltdat,Conds),mean(value))
Results = rbind(Results,Ps)

############################### First Table: Remember Rates Exp2
Meltdat=melt(d[d$group == "exp2",], id.vars = c("age2","subj"), 
             measure.vars = c("R_corr_rate", "R_incorr_rate"), 
             variable.name = "condition")
Meltdat$age2 = as.factor(Meltdat$age2) 
Meltdat$condition = factor(Meltdat$condition, 
                           levels = c("R_corr_rate", "R_incorr_rate"),labels = c("Remember_Cor","Remember_Inc"))


Meltdat$Conds = paste(Meltdat$age2,Meltdat$condition,sep = " ")
pwc <- Meltdat  %>%
  pairwise_t_test(value ~ Conds, pool.sd = FALSE,
                  p.adjust.method = "bonferroni")
Ps = pwc
Ps$exp = unique("EXp2")
summarise(group_by(Meltdat,Conds),mean(value))
Results = rbind(Results,Ps)
############################### First Table: Remember Rates Exp3
Meltdat=melt(d[d$group == "exp3",], id.vars = c("age2","subj"), 
             measure.vars = c("R_corr_rate", "R_incorr_rate"), 
             variable.name = "condition")
Meltdat$age2 = as.factor(Meltdat$age2) 
Meltdat$condition = factor(Meltdat$condition, 
                           levels = c("R_corr_rate", "R_incorr_rate"),labels = c("Remember_Cor","Remember_Inc"))
Meltdat$Conds = paste(Meltdat$age2,Meltdat$condition,sep = " ")
pwc <- Meltdat  %>%
  pairwise_t_test(value ~ Conds, pool.sd = FALSE,
                  p.adjust.method = "bonferroni")

Ps = pwc
Ps$exp = unique("EXp3")
Results = rbind(Results,Ps)
summarise(group_by(Meltdat,Conds),mean(value))
write.csv(Results,"Stats_Remember.csv")






############################### Second Table: Treasure Rates old----
Results = NULL
A = t.test(d$Treas_corr_rate[d$group=="exp1"&d$age2 == "6_7_years"],d$Treas_incorr_rate[d$group=="exp1"&d$age2 == "6_7_years"],paired = T)
Meltdat=melt(d[d$group == "exp1",], id.vars = c("age2","subj"), 
             measure.vars = c("Treas_corr_rate", "Treas_incorr_rate"), 
             variable.name = "condition")
Meltdat$age2 = as.factor(Meltdat$age2) 
Meltdat$condition = factor(Meltdat$condition, 
                           levels = c("Treas_corr_rate", "Treas_incorr_rate"),labels = c("Treas_Cor","Treas_Inc"))


Meltdat$Conds = paste(Meltdat$group,Meltdat$age2,Meltdat$condition,sep = " ")
pwc <- Meltdat  %>%
  pairwise_t_test(value ~ Conds, pool.sd = FALSE,
                  p.adjust.method = "bonferroni")
Ps = pwc
Ps$exp = unique("EXp1")
summarise(group_by(Meltdat,Conds),mean(value))
Results = rbind(Results,Ps)

############################### First Table: Treasure Rates Exp2
Meltdat=melt(d[d$group == "exp2",], id.vars = c("age2","subj"), 
             measure.vars = c("Treas_corr_rate", "Treas_incorr_rate"), 
             variable.name = "condition")
Meltdat$age2 = as.factor(Meltdat$age2) 
Meltdat$condition = factor(Meltdat$condition, 
                           levels = c("Treas_corr_rate", "Treas_incorr_rate"),labels = c("Treas_Cor","Treas_Inc"))


Meltdat$Conds = paste(Meltdat$age2,Meltdat$condition,sep = " ")
pwc <- Meltdat  %>%
  pairwise_t_test(value ~ Conds, pool.sd = FALSE,
                  p.adjust.method = "bonferroni")
Ps = pwc
Ps$exp = unique("EXp2")
summarise(group_by(Meltdat,Conds),mean(value))
Results = rbind(Results,Ps)
############################### First Table: Treasure Rates Exp3
Meltdat=melt(d[d$group == "exp3",], id.vars = c("age2","subj"), 
             measure.vars = c("Treas_corr_rate", "Treas_incorr_rate"), 
             variable.name = "condition")
Meltdat$age2 = as.factor(Meltdat$age2) 
Meltdat$condition = factor(Meltdat$condition, 
                           levels = c("Treas_corr_rate", "Treas_incorr_rate"),labels = c("Treas_Cor","Treas_Inc"))
Meltdat$Conds = paste(Meltdat$age2,Meltdat$condition,sep = " ")
pwc <- Meltdat  %>%
  pairwise_t_test(value ~ Conds, pool.sd = FALSE,
                  p.adjust.method = "bonferroni")

Ps = pwc
Ps$exp = unique("EXp3")
Results = rbind(Results,Ps)
summarise(group_by(Meltdat,Conds),mean(value))
write.csv(Results,"Stats_Treasure.csv")

############################### Case-wise Deletions Report ----
data=subset(d[d$group=="exp1",], select=c("subj", "Gender", "age2",
                                          "Treas_R_match", "Treas_F_match",  
                                          "Treas_R_nonmatch", "Treas_F_nonmatch"))
nrow(data)-nrow(data[complete.cases(data),]) #how many people removed for NA
# data[!complete.cases(data),]
data=data[complete.cases(data),]

anova_data=melt(data, id.vars = c("Gender", "age2","subj"), 
                measure.vars = c("Treas_R_match", "Treas_F_match",  
                                 "Treas_R_nonmatch", "Treas_F_nonmatch"), 
                variable.name = "Resp_type")

anova_data$Trial_type = case_when(anova_data$Resp_type == "Treas_R_match" ~ "match",
                                  anova_data$Resp_type == "Treas_F_match" ~ "match",
                                  anova_data$Resp_type == "Treas_R_nonmatch" ~ "nonmatch",
                                  anova_data$Resp_type == "Treas_F_nonmatch" ~ "nonmatch")
anova_data$Resp_type = factor(anova_data$Resp_type , levels = c("Treas_R_match", "Treas_F_match","Treas_R_nonmatch", "Treas_F_nonmatch"),
                                                      labels = c("Remember","Familiar","Remember","Familiar"))

results=as.data.frame(ezANOVA(data=anova_data, dv=value,wid=.(subj),within=.(Resp_type,Trial_type),
                              between=.(age2), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num]=lapply(results[is.num], round, 3)
results


data=subset(d[d$group=="exp2",], select=c("subj", "Gender", "age2",
                                          "Treas_R_match", "Treas_F_match",  
                                          "Treas_R_nonmatch", "Treas_F_nonmatch"))
nrow(data)-nrow(data[complete.cases(data),]) #how many people removed for NA
# data[!complete.cases(data),]
data=data[complete.cases(data),]

anova_data=melt(data, id.vars = c("Gender", "age2","subj"), 
                measure.vars = c("Treas_R_match", "Treas_F_match",  
                                 "Treas_R_nonmatch", "Treas_F_nonmatch"), 
                variable.name = "Resp_type")

anova_data$Trial_type = case_when(anova_data$Resp_type == "Treas_R_match" ~ "match",
                                  anova_data$Resp_type == "Treas_F_match" ~ "match",
                                  anova_data$Resp_type == "Treas_R_nonmatch" ~ "nonmatch",
                                  anova_data$Resp_type == "Treas_F_nonmatch" ~ "nonmatch")
anova_data$Resp_type = factor(anova_data$Resp_type , levels = c("Treas_R_match", "Treas_F_match","Treas_R_nonmatch", "Treas_F_nonmatch"),
                              labels = c("Remember","Familiar","Remember","Familiar"))

results=as.data.frame(ezANOVA(data=anova_data, dv=value,wid=.(subj),within=.(Resp_type,Trial_type),
                              between=.(age2), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num]=lapply(results[is.num], round, 3)
results


data=subset(d[d$group=="exp3",], select=c("subj", "Gender", "age2",
                                          "Treas_R_match", "Treas_F_match",  
                                          "Treas_R_nonmatch", "Treas_F_nonmatch"))
nrow(data)-nrow(data[complete.cases(data),]) #how many people removed for NA
# data[!complete.cases(data),]
data=data[complete.cases(data),]

anova_data=melt(data, id.vars = c("Gender", "age2","subj"), 
                measure.vars = c("Treas_R_match", "Treas_F_match",  
                                 "Treas_R_nonmatch", "Treas_F_nonmatch"), 
                variable.name = "Resp_type")

anova_data$Trial_type = case_when(anova_data$Resp_type == "Treas_R_match" ~ "match",
                                  anova_data$Resp_type == "Treas_F_match" ~ "match",
                                  anova_data$Resp_type == "Treas_R_nonmatch" ~ "nonmatch",
                                  anova_data$Resp_type == "Treas_F_nonmatch" ~ "nonmatch")
anova_data$Resp_type = factor(anova_data$Resp_type , levels = c("Treas_R_match", "Treas_F_match","Treas_R_nonmatch", "Treas_F_nonmatch"),
                              labels = c("Remember","Familiar","Remember","Familiar"))

results=as.data.frame(ezANOVA(data=anova_data, dv=value,wid=.(subj),within=.(Resp_type,Trial_type),
                              between=.(age2), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num]=lapply(results[is.num], round, 3)
results



############################################# Cohen's d and bonferroni corrections Exp1 - Accuracy------
data=subset(d[which(d$group=="exp1"),], select=c(subj, match_pc, nonmatch_pc, Gender, BetOrder, age2))


anova_data=melt(data, id.vars = c("Gender", "age2","BetOrder","subj"), 
                variable.name = "condition")


as.data.frame(summarise(group_by(anova_data,age2,condition),M = round(mean(value),2),SD = round(sd(value),2)))

results=as.data.frame(ezANOVA(data=anova_data, dv=value,wid=.(subj),within=.(condition),
                              between=.(age2), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num]=lapply(results[is.num], round, 3)
results

#-------------> age main effect

dat_Main = as.data.frame(summarise(group_by(anova_data,subj,age2),value = mean(value)))

# Pairwise Comparisons:
emmeans_test(dat_Main,value~age2,p.adjust.method = "bonferroni")
pwc <- dat_Main  %>%
  pairwise_t_test(value ~ age2, pool.sd = F,var.equal = T,paired = F,
                  p.adjust.method = "bonferroni")
as.data.frame(pwc)

# Summary
as.data.frame(summarise(group_by(dat_Main,age2),M = round(mean(value),2),SD = round(sd(value),2)))

# Cohen's d
CD =as.data.frame(cohens_d(dat_Main,value~age2, var.equal = T,paired = F))
CD$effsize = round(CD$effsize,2)
CD
#-------------> condition main effect
dat_Main = as.data.frame(summarise(group_by(anova_data,subj,condition),value = mean(value)))

# Pairwise Comparisons:
emmeans_test(dat_Main,value~condition,p.adjust.method = "bonferroni")
pwc <- dat_Main  %>%
  pairwise_t_test(value ~ condition, pool.sd = F,var.equal = T,paired = T,
                  p.adjust.method = "bonferroni")
pwc
# Summary
as.data.frame(summarise(group_by(dat_Main,condition),M = round(mean(value),2),SD = round(sd(value),2)))

# Cohen's d
CD =as.data.frame(cohens_d(dat_Main,value~condition, var.equal = T,paired = T))
CD$effsize = round(CD$effsize,2)
CD
############################################# Cohen's d and bonferroni corrections Exp1 - Remember------
data=subset(d[which(d$group=="exp1"),], select=c(subj, R_match_rate, R_nonmatch_rate, Gender, BetOrder, age2))


anova_data=melt(data, id.vars = c("Gender", "age2","BetOrder","subj"), 
                variable.name = "condition")


as.data.frame(summarise(group_by(anova_data,age2,condition),M = round(mean(value),2),SD = round(sd(value),2)))

results=as.data.frame(ezANOVA(data=anova_data, dv=value,wid=.(subj),within=.(condition),
                              between=.(age2), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num]=lapply(results[is.num], round, 3)
results

#-------------> age main effect

dat_Main = as.data.frame(summarise(group_by(anova_data,subj,age2),value = mean(value)))

# Pairwise Comparisons:
emmeans_test(dat_Main,value~age2,p.adjust.method = "bonferroni")
pwc <- dat_Main  %>%
  pairwise_t_test(value ~ age2, pool.sd = F,var.equal = T,paired = F,
                  p.adjust.method = "bonferroni")
as.data.frame(pwc)

# Summary
as.data.frame(summarise(group_by(dat_Main,age2),M = round(mean(value),2),SD = round(sd(value),2)))

# Cohen's d
CD =as.data.frame(cohens_d(dat_Main,value~age2, var.equal = T,paired = F))
CD$effsize = round(CD$effsize,2)
CD

#-------------> condition main effect
dat_Main = as.data.frame(summarise(group_by(anova_data,subj,condition),value = mean(value)))

# Pairwise Comparisons:
emmeans_test(dat_Main,value~condition,p.adjust.method = "bonferroni")
pwc <- dat_Main  %>%
  pairwise_t_test(value ~ condition, pool.sd = F,var.equal = T,paired = T,
                  p.adjust.method = "bonferroni")
as.data.frame(pwc)

# Summary
as.data.frame(summarise(group_by(dat_Main,condition),M = round(mean(value),2),SD = round(sd(value),2)))

# Cohen's d
CD =as.data.frame(cohens_d(dat_Main,value~condition, var.equal = T,paired = T))
CD$effsize = round(CD$effsize,2)
CD
#-------------> Interaction effect

as.data.frame(summarise(group_by(anova_data,age2,condition),M = round(mean(value),2),SD = round(sd(value),2)))

# differences
dat_Main = data
dat_Main$value = dat_Main$R_match_rate - dat_Main$R_nonmatch_rate
pwc <- dat_Main  %>%
  pairwise_t_test(value ~ age2, pool.sd = F,var.equal = T,paired = F,
                  p.adjust.method = "bonferroni")
as.data.frame(pwc)

as.data.frame(summarise(group_by(dat_Main,age2),M = round(mean(value),2),SD = round(sd(value),2)))


# Cohen's d
CD =as.data.frame(cohens_d(dat_Main,value~age2, var.equal = T,paired = F))
CD$effsize = round(CD$effsize,2)
CD


# All Pairwise
anova_data$intcond = as.factor(paste(anova_data$age2,anova_data$condition,sep = "_"))

as.data.frame(summarise(group_by(anova_data,intcond),M = round(mean(value),2),SD = round(sd(value),2)))
# Pairwise Comparisons:
pwc <- anova_data  %>%
  pairwise_t_test(value ~ intcond, pool.sd = F,var.equal = T,paired = F,
                  p.adjust.method = "bonferroni",
                  comparisons = list(c("6_7_years_R_match_rate","Adults_R_match_rate"),
                                     c("6_7_years_R_match_rate","9_10_years_R_match_rate"),
                                     c("9_10_years_R_match_rate","Adults_R_match_rate")))
pwc$p = round(pwc$p,3)
pwc$p.adj = round(pwc$p.adj,3)
as.data.frame(pwc)

CD =as.data.frame(cohens_d(anova_data,value~intcond, var.equal = T,paired = F,
                           comparisons = list(c("6_7_years_R_match_rate","Adults_R_match_rate"),
                                              c("6_7_years_R_match_rate","9_10_years_R_match_rate"),
                                              c("9_10_years_R_match_rate","Adults_R_match_rate")) ))
CD$effsize = round(CD$effsize,2)
CD

dat_main  = anova_data[anova_data$condition=="R_nonmatch_rate",]
results=as.data.frame(ezANOVA(data=dat_main, dv=value,wid=.(subj),
                              between=.(age2), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num]=lapply(results[is.num], round, 3)
results



pwc <- anova_data  %>%
  pairwise_t_test(value ~ intcond, pool.sd = F,var.equal = T,paired = T,
                  comparisons = list(c("6_7_years_R_match_rate","6_7_years_R_nonmatch_rate"),
                                     c("9_10_years_R_match_rate","9_10_years_R_nonmatch_rate"),
                                     c("Adults_R_match_rate","Adults_R_nonmatch_rate")),
                  p.adjust.method = "bonferroni")
pwc

############################################# Cohen's d and bonferroni corrections Exp1 - Treasure------
data=subset(d[which(d$group=="exp1"),], select=c(subj, Treas_match_rate, Treas_nonmatch_rate, Gender, BetOrder, age2))


anova_data=melt(data, id.vars = c("Gender", "age2","BetOrder","subj"), 
                variable.name = "condition")


as.data.frame(summarise(group_by(anova_data,age2,condition),M = round(mean(value),2),SD = round(sd(value),2)))

results=as.data.frame(ezANOVA(data=anova_data, dv=value,wid=.(subj),within=.(condition),
                              between=.(age2), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num]=lapply(results[is.num], round, 3)
results

#-------------> condition main effect
dat_Main = as.data.frame(summarise(group_by(anova_data,subj,condition),value = mean(value)))

# Pairwise Comparisons:
emmeans_test(dat_Main,value~condition,p.adjust.method = "bonferroni")
pwc <- dat_Main  %>%
  pairwise_t_test(value ~ condition, pool.sd = F,var.equal = T,paired = T,
                  p.adjust.method = "bonferroni")
pwc$p = round(pwc$p,3)
pwc$p.adj = round(pwc$p.adj,3)
as.data.frame(pwc)

# Summary
as.data.frame(summarise(group_by(dat_Main,condition),M = round(mean(value),2),SD = round(sd(value),2)))

# Cohen's d
CD =as.data.frame(cohens_d(dat_Main,value~condition, var.equal = T,paired = T))
CD$effsize = round(CD$effsize,2)
CD
#-------------> Interaction effect

# All Pairwise
anova_data$intcond = as.factor(paste(anova_data$age2,anova_data$condition,sep = "_"))

as.data.frame(summarise(group_by(anova_data,intcond),M = round(mean(value),2),SD = round(sd(value),2)))
# Pairwise Comparisons:
pwc <- anova_data  %>%
  pairwise_t_test(value ~ intcond, pool.sd = F,var.equal = T,paired = F,
                  p.adjust.method = "bonferroni",
                  comparisons = list(c("6_7_years_Treas_match_rate","Adults_Treas_match_rate"),
                                     c("6_7_years_Treas_match_rate","9_10_years_Treas_match_rate"),
                                     c("9_10_years_Treas_match_rate","Adults_Treas_match_rate")))
pwc$p = round(pwc$p,3)
pwc$p.adj = round(pwc$p.adj,3)
as.data.frame(pwc)

CD =as.data.frame(cohens_d(anova_data,value~intcond, var.equal = T,paired = F,
                           comparisons = list(c("6_7_years_Treas_match_rate","Adults_Treas_match_rate"),
                                              c("6_7_years_Treas_match_rate","9_10_years_Treas_match_rate"),
                                              c("9_10_years_Treas_match_rate","Adults_Treas_match_rate")) ))
CD$effsize = round(CD$effsize,2)
CD

dat_main  = anova_data[anova_data$condition=="Treas_nonmatch_rate",]
results=as.data.frame(ezANOVA(data=dat_main, dv=value,wid=.(subj),
                              between=.(age2), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num]=lapply(results[is.num], round, 3)
results

pwc <- anova_data  %>%
  pairwise_t_test(value ~ intcond, pool.sd = F,var.equal = T,paired = T,
                  comparisons = list(c("6_7_years_Treas_match_rate","6_7_years_Treas_nonmatch_rate"),
                                     c("9_10_years_Treas_match_rate","9_10_years_Treas_nonmatch_rate"),
                                     c("Adults_Treas_match_rate","Adults_Treas_nonmatch_rate")),
                  p.adjust.method = "bonferroni")
pwc$p = round(pwc$p,3)
pwc$p.adj = round(pwc$p.adj,3)
as.data.frame(pwc)

############################################# Cohen's d and bonferroni corrections Exp1 - Treasure Age X Response------
data=subset(d[which(d$group=="exp1"),], select=c(subj, Treas_R, Treas_F, Gender, age2))
nrow(data)-nrow(data[complete.cases(data),]) #how many people removed for NA
data[!complete.cases(data),]
data=data[complete.cases(data),]

anova_data=melt(data, id.vars = c("Gender", "age2","subj"), 
                variable.name = "condition")


as.data.frame(summarise(group_by(anova_data,age2,condition),M = round(mean(value),2),SD = round(sd(value),2)))

results=as.data.frame(ezANOVA(data=anova_data, dv=value,wid=.(subj),within=.(condition),
                              between=.(age2), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num]=lapply(results[is.num], round, 3)
results

#-------------> condition main effect
dat_Main = as.data.frame(summarise(group_by(anova_data,subj,condition),value = mean(value)))

# Pairwise Comparisons:
pwc <- dat_Main  %>%
  pairwise_t_test(value ~ condition, pool.sd = F,var.equal = T,paired = T,
                  p.adjust.method = "bonferroni")
pwc$p = round(pwc$p,3)
pwc$p.adj = round(pwc$p.adj,3)
as.data.frame(pwc)

# Summary
as.data.frame(summarise(group_by(dat_Main,condition),M = round(mean(value),2),SD = round(sd(value),2)))

# Cohen's d
CD =as.data.frame(cohens_d(dat_Main,value~condition, var.equal = T,paired = T))
CD$effsize = round(CD$effsize,2)
CD
# ------------> From 100%
as.data.frame(summarise(group_by(anova_data,age2,condition),M = round(mean(value),2),SD = round(sd(value),2)))
dat_main = anova_data
dat_main = dat_main[dat_main$condition=="Treas_R",]
t.test(1-dat_main$value[dat_main$age2=="6_7_years"])
t.test(1-dat_main$value[dat_main$age2=="9_10_years"])
t.test(1-dat_main$value[dat_main$age2=="Adults"])
dat_main = anova_data
dat_main = dat_main[dat_main$condition=="Treas_F",]
t.test(dat_main$value[dat_main$age2=="6_7_years"])
t.test(dat_main$value[dat_main$age2=="9_10_years"])
t.test(dat_main$value[dat_main$age2=="Adults"])


############################################# Cohen's d and bonferroni corrections Exp2 - Accuracy------
data=subset(d[which(d$group=="exp2"),], select=c(subj, match_pc, nonmatch_pc, Gender, BetOrder, age2))


anova_data=melt(data, id.vars = c("Gender", "age2","BetOrder","subj"), 
                variable.name = "condition")


as.data.frame(summarise(group_by(anova_data,age2,condition),M = round(mean(value),2),SD = round(sd(value),2)))

results=as.data.frame(ezANOVA(data=anova_data, dv=value,wid=.(subj),within=.(condition),
                              between=.(age2), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num]=lapply(results[is.num], round, 3)
results

#-------------> condition main effect
dat_Main = as.data.frame(summarise(group_by(anova_data,subj,condition),value = mean(value)))

# Pairwise Comparisons:
pwc <- dat_Main  %>%
  pairwise_t_test(value ~ condition, pool.sd = F,var.equal = T,paired = T,
                  p.adjust.method = "bonferroni")
pwc$p = round(pwc$p,3)
pwc$p.adj = round(pwc$p.adj,3)
as.data.frame(pwc)

# Summary
as.data.frame(summarise(group_by(dat_Main,condition),M = round(mean(value),2),SD = round(sd(value),2)))

# Cohen's d
CD =as.data.frame(cohens_d(dat_Main,value~condition, var.equal = T,paired = T))
CD$effsize = round(CD$effsize,2)
CD

############################################# Cohen's d and bonferroni corrections Exp2 - Remember------
data=subset(d[which(d$group=="exp2"),], select=c(subj, R_match_rate, R_nonmatch_rate, Gender, BetOrder, age2))


anova_data=melt(data, id.vars = c("Gender", "age2","BetOrder","subj"), 
                variable.name = "condition")


as.data.frame(summarise(group_by(anova_data,age2,condition),M = round(mean(value),2),SD = round(sd(value),2)))

results=as.data.frame(ezANOVA(data=anova_data, dv=value,wid=.(subj),within=.(condition),
                              between=.(age2), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num]=lapply(results[is.num], round, 3)
results

anova_data$intcond = as.factor(paste(anova_data$age2,anova_data$condition,sep = "_"))
pwc <- anova_data  %>%
  pairwise_t_test(value ~ intcond, pool.sd = F,,paired = T,var.equal = T,
                  comparisons = list(c("6_7_years_R_match_rate","6_7_years_R_nonmatch_rate"),
                                     c("Adults_R_match_rate","Adults_R_nonmatch_rate")),
                  p.adjust.method = "bonferroni")
pwc

CD =as.data.frame(cohens_d(anova_data,value~intcond, var.equal = T,paired = T,
                           comparisons = list(c("6_7_years_R_match_rate","6_7_years_R_nonmatch_rate"),
                                              c("Adults_R_match_rate","Adults_R_nonmatch_rate"))))
CD$effsize = round(CD$effsize,2)
CD


pwc <- anova_data  %>%
  pairwise_t_test(value ~ intcond, pool.sd = F,var.equal = T,paired = F,
                  p.adjust.method = "bonferroni",
                  comparisons = list(c("6_7_years_R_match_rate","Adults_R_match_rate"),
                                     c("6_7_years_R_nonmatch_rate","Adults_R_nonmatch_rate")))
pwc$p = round(pwc$p,3)
pwc$p.adj = round(pwc$p.adj,3)
as.data.frame(pwc)

CD =as.data.frame(cohens_d(anova_data,value~intcond, var.equal = T,paired = F,
                           comparisons = list(c("6_7_years_R_match_rate","Adults_R_match_rate"),
                                              c("6_7_years_R_nonmatch_rate","Adults_R_nonmatch_rate"))))
CD$effsize = round(CD$effsize,2)
CD
############################################# Cohen's d and bonferroni corrections Exp2 - Remember correct vs. Incorrect------
td=read.table('trialdata_subjremoved.xls', header=TRUE, sep="\t", strip.white = TRUE)
td = td[td$Group==2 & td$age2 == "child",]
td$remember = case_when(td$rfResp=="remember"~1,
                        TRUE ~ 0)
dattemp =as.data.frame(summarise(group_by(td,subj,accuracy),R_Rate = sum(remember)/n()))

pwc <- dattemp  %>%
  pairwise_t_test(R_Rate ~ accuracy, pool.sd = F,var.equal = T,paired = T,
                  p.adjust.method = "bonferroni")
pwc$p = round(pwc$p,3)
pwc$p.adj = round(pwc$p.adj,3)
as.data.frame(pwc)

CD =as.data.frame(cohens_d(dattemp,R_Rate ~ accuracy, var.equal = T,paired = T))
CD$effsize = round(CD$effsize,2)
CD

as.data.frame(summarise(group_by(dattemp,accuracy),M = round(mean(R_Rate),2),SD = round(sd(R_Rate),2)))

Vals = as.data.frame(summarise(group_by(dattemp,accuracy),M = mean(R_Rate),SD = sd(R_Rate)))

data=subset(d[which(d$group=="exp2"),], select=c(subj, R_corr_rate, R_incorr_rate, Gender, age2))
data=data[complete.cases(data),]
data = data[data$age2=="6_7_years",]
anova_data=melt(data, id.vars = c("Gender", "age2","subj"), 
                variable.name = "condition")
t.test(data$R_corr_rate,data$R_incorr_rate,paired = T, var.equal = T)
cohens_d(anova_data, value~condition, paired = T, var.equal = T,
         comparisons = list(c("R_corr_rate","R_incorr_rate")))
cohen.d(anova_data$value,anova_data$condition,paired = T)
cohens_d(anova_data$value,anova_data$condition,paired = T)
Vals = as.data.frame(summarise(group_by(anova_data,age2,condition),M = mean(value),SD = sd(value)))
X = data$R_corr_rate
Y = data$R_incorr_rate
(mean(X)-mean(Y))/sqrt((sd(X)^2+sd(Y)^2) - 2*sd(Y)*sd(X)*cor(Y,X))

as.data.frame(summarise(group_by(anova_data,age2,condition),M = round(mean(value),2),SD = round(sd(value),2)))

results=as.data.frame(ezANOVA(data=anova_data, dv=value,wid=.(subj),within=.(condition),
                              between=.(age2), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num]=lapply(results[is.num], round, 3)
results

anova_data$intcond = as.factor(paste(anova_data$age2,anova_data$condition,sep = "_"))
pwc <- anova_data  %>%
  pairwise_t_test(value ~ intcond, pool.sd = F,var.equal = T,paired = T,
                  comparisons = list(c("6_7_years_R_match_corr_rate","6_7_years_R_match_incorr_rate"),
                                     c("6_7_years_R_nonmatch_corr_rate","6_7_years_R_nonmatch_incorr_rate")),
                                     # c("Adults_R_match_corr_rate","Adults_R_match_incorr_rate"),
                                     # c("Adults_R_nonmatch_corr_rate","Adults_R_nonmatch_incorr_rate"),
                                     # c("6_7_years_R_match_corr_rate","6_7_years_R_nonmatch_corr_rate"),
                                     # c("Adults_R_match_corr_rate","Adults_R_nonmatch_corr_rate")),
                  p.adjust.method = "bonferroni")
pwc$p = round(pwc$p,3)
pwc$p.adj = round(pwc$p.adj,3)
as.data.frame(pwc)


############################################# Cohen's d and bonferroni corrections Exp2 - Treasure------
data=subset(d[which(d$group=="exp2"),], select=c(subj, Treas_match_rate, Treas_nonmatch_rate, Gender, BetOrder, age2))


anova_data=melt(data, id.vars = c("Gender", "age2","BetOrder","subj"), 
                variable.name = "condition")


as.data.frame(summarise(group_by(anova_data,age2,condition),M = round(mean(value),2),SD = round(sd(value),2)))

results=as.data.frame(ezANOVA(data=anova_data, dv=value,wid=.(subj),within=.(condition),
                              between=.(age2), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num]=lapply(results[is.num], round, 3)
results

#-------------> Interaction effect

anova_data$intcond = as.factor(paste(anova_data$age2,anova_data$condition,sep = "_"))
as.data.frame(summarise(group_by(anova_data,intcond),M = round(mean(value),2),SD = round(sd(value),2)))

pwc <- anova_data  %>%
  pairwise_t_test(value ~ intcond, pool.sd = F,var.equal = T,paired = T,
                  comparisons = list(c("6_7_years_Treas_match_rate","6_7_years_Treas_nonmatch_rate"),
                                     c("Adults_Treas_match_rate","Adults_Treas_nonmatch_rate")),
                  p.adjust.method = "bonferroni")
pwc

CD =as.data.frame(cohens_d(anova_data,value~intcond, var.equal = T,paired = T,
                           comparisons = list(c("6_7_years_Treas_match_rate","6_7_years_Treas_nonmatch_rate"),
                                              c("Adults_Treas_match_rate","Adults_Treas_nonmatch_rate"))))
CD$effsize = round(CD$effsize,2)
CD


# All Pairwise
pwc <- anova_data  %>%
  pairwise_t_test(value ~ intcond, pool.sd = F,var.equal = T,paired = F,
                  comparisons = list(c("6_7_years_Treas_match_rate","Adults_Treas_match_rate"),
                                     c("6_7_years_Treas_nonmatch_rate","Adults_Treas_nonmatch_rate")),
                  p.adjust.method = "bonferroni")
pwc


CD =as.data.frame(cohens_d(anova_data,value~intcond, var.equal = T,paired = F,
                           comparisons = list(c("6_7_years_Treas_match_rate","Adults_Treas_match_rate"))))
CD$effsize = round(CD$effsize,2)
CD


############################################# Cohen's d and bonferroni corrections Exp2 - Treasure correct vs. Incorrest------

data=subset(d[which(d$group=="exp2"),], select=c(subj, Treas_corr_rate, Treas_incorr_rate, Gender, age2))
data=data[complete.cases(data),]
data = data[data$age2=="6_7_years",]
anova_data=melt(data, id.vars = c("Gender", "age2","subj"), 
                variable.name = "condition")
t.test(data$Treas_corr_rate,data$Treas_incorr_rate,paired = T, var.equal = T)
cohens_d(anova_data, value~condition, paired = T, var.equal = T)
cohen.d(anova_data$value,anova_data$condition,paired = T)
X = data$Treas_corr_rate
Y = data$Treas_incorr_rate
(mean(X)-mean(Y))/sqrt((sd(X)^2+sd(Y)^2) - 2*sd(Y)*sd(X)*cor(Y,X))


td=read.table('trialdata_subjremoved.xls', header=TRUE, sep="\t", strip.white = TRUE)
td = td[td$Group==2 & td$age2 == "child",]
td$treasure = case_when(td$betResp=="treasure"~1,
                        TRUE ~ 0)
dattemp =as.data.frame(summarise(group_by(td,subj,accuracy),Treas_Rate = sum(treasure)/n()))

pwc <- dattemp  %>%
  pairwise_t_test(Treas_Rate ~ accuracy, pool.sd = F,var.equal = T,paired = T,
                  p.adjust.method = "bonferroni")
pwc$p = round(pwc$p,3)
pwc$p.adj = round(pwc$p.adj,3)
as.data.frame(pwc)

CD =as.data.frame(cohens_d(dattemp,Treas_Rate ~ accuracy, var.equal = T,paired = T))
CD$effsize = round(CD$effsize,2)
CD

as.data.frame(summarise(group_by(dattemp,accuracy),M = round(mean(Treas_Rate),2),SD = round(sd(Treas_Rate),2)))


data=subset(d[which(d$group=="exp2"),], select=c(subj, Treas_match_corr_rate, Treas_match_incorr_rate, Treas_nonmatch_corr_rate, Treas_nonmatch_incorr_rate, Gender, age2))
data=data[complete.cases(data),]

anova_data=melt(data, id.vars = c("Gender", "age2","subj"), 
                variable.name = "condition")

anova_data$intcond = as.factor(paste(anova_data$age2,anova_data$condition,sep = "_"))
as.data.frame(summarise(group_by(anova_data,intcond),M = round(mean(value),2),SD = round(sd(value),2)))

pwc <- anova_data  %>%
  pairwise_t_test(value ~ intcond, pool.sd = T,paired = T,comparisons = list(c("6_7_years_Treas_match_corr_rate","6_7_years_Treas_match_incorr_rate"),
                                                                             c("6_7_years_Treas_nonmatch_corr_rate","6_7_years_Treas_nonmatch_incorr_rate")),
                  # c("Adults_R_match_corr_rate","Adults_R_match_incorr_rate"),
                  # c("Adults_R_nonmatch_corr_rate","Adults_R_nonmatch_incorr_rate"),
                  # c("6_7_years_R_match_corr_rate","6_7_years_R_nonmatch_corr_rate"),
                  # c("Adults_R_match_corr_rate","Adults_R_nonmatch_corr_rate")),
                  p.adjust.method = "bonferroni")
pwc

Inds = anova_data$intcond %in% c("6_7_years_Treas_match_corr_rate","6_7_years_Treas_match_incorr_rate")
CD = cohen.d(anova_data$value[Inds],anova_data$intcond[Inds])
round(CD$cohen.d,2)
Inds = anova_data$intcond %in% c("6_7_years_Treas_nonmatch_corr_rate","6_7_years_Treas_nonmatch_incorr_rate")
CD = cohen.d(anova_data$value[Inds],anova_data$intcond[Inds])
round(CD$cohen.d,2)



############################################# Cohen's d and bonferroni corrections Exp3 - Accuracy------
data=subset(d[which(d$group=="exp3"),], select=c(subj, match_pc, nonmatch_pc, Gender, BetOrder, age2))


anova_data=melt(data, id.vars = c("Gender", "age2","BetOrder","subj"), 
                variable.name = "condition")


as.data.frame(summarise(group_by(anova_data,age2,condition),M = round(mean(value),2),SD = round(sd(value),2)))

results=as.data.frame(ezANOVA(data=anova_data, dv=value,wid=.(subj),within=.(condition),
                              between=.(age2), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num]=lapply(results[is.num], round, 3)
results

#-------------> condition main effect
dat_Main = as.data.frame(summarise(group_by(anova_data,subj,condition),value = mean(value)))

# Pairwise Comparisons:
pwc <- dat_Main  %>%
  pairwise_t_test(value ~ condition, pool.sd = F,var.equal = T,paired = T,
                  p.adjust.method = "bonferroni")
pwc$p = round(pwc$p,3)
pwc$p.adj = round(pwc$p.adj,3)
as.data.frame(pwc)

# Summary
as.data.frame(summarise(group_by(dat_Main,condition),M = round(mean(value),2),SD = round(sd(value),2)))

# Cohen's d
CD =as.data.frame(cohens_d(dat_Main,value~condition, var.equal = T,paired = T))
CD$effsize = round(CD$effsize,2)
CD
############################################# Cohen's d and bonferroni corrections Exp3 - Remember------
data=subset(d[which(d$group=="exp3"),], select=c(subj, R_match_rate, R_nonmatch_rate, Gender, BetOrder, age2))


anova_data=melt(data, id.vars = c("Gender", "age2","BetOrder","subj"), 
                variable.name = "condition")


as.data.frame(summarise(group_by(anova_data,age2,condition),M = round(mean(value),2),SD = round(sd(value),2)))

results=as.data.frame(ezANOVA(data=anova_data, dv=value,wid=.(subj),within=.(condition),
                              between=.(age2), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num]=lapply(results[is.num], round, 3)
results

anova_data$intcond = as.factor(paste(anova_data$age2,anova_data$condition,sep = "_"))
pwc <- anova_data  %>%
  pairwise_t_test(value ~ intcond, pool.sd = T,paired = T,var.equal = T,
                  comparisons = list(c("6_7_years_R_match_rate","6_7_years_R_nonmatch_rate"),
                                     c("Adults_R_match_rate","Adults_R_nonmatch_rate")),
                  p.adjust.method = "bonferroni")
pwc

CD =as.data.frame(cohens_d(anova_data,value~intcond, var.equal = T,paired = T,
                           comparisons = list(c("6_7_years_R_match_rate","6_7_years_R_nonmatch_rate"),
                                              c("Adults_R_match_rate","Adults_R_nonmatch_rate"))))
CD$effsize = round(CD$effsize,2)
CD

data$value = data$R_nonmatch_rate - data$R_match_rate

pwc <- data  %>%
  pairwise_t_test(value ~ age2, pool.sd = T,paired = F,
                  p.adjust.method = "bonferroni")
pwc

data$value = data$R_nonmatch_rate - data$R_match_rate
t.test(data$value[data$age2=="6_7_years"],data$value[data$age2=="Adults"],var.equal = T,paired = F)
summarise(group_by(data,age2),M = round(mean(value),2),SD = round(sd(value),2))

CD =as.data.frame(cohens_d(data,value~age2, var.equal = T,paired = F,
                           comparisons = list(c("6_7_years","Adults"))))
CD$effsize = round(CD$effsize,2)
CD
t.test(data$value[data$age2=="6_7_years"])
t.test(data$value[data$age2=="Adults"])
as.data.frame(cohens_d(data[data$age2=="6_7_years",],value~0,var.equal = T))
as.data.frame(cohens_d(anova_data[anova_data$age2=="6_7_years",],value~condition,var.equal = T,paired = T))
as.data.frame(cohens_d(data[data$age2=="Adults",],value~0,var.equal = T))
############################################# Cohen's d and bonferroni corrections Exp3 - Treasure------
data=subset(d[which(d$group=="exp3"),], select=c(subj, Treas_match_rate, Treas_nonmatch_rate, Gender, BetOrder, age2))


anova_data=melt(data, id.vars = c("Gender", "age2","BetOrder","subj"), 
                variable.name = "condition")


as.data.frame(summarise(group_by(anova_data,age2,condition),M = round(mean(value),2),SD = round(sd(value),2)))

results=as.data.frame(ezANOVA(data=anova_data, dv=value,wid=.(subj),within=.(condition),
                              between=.(age2), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num]=lapply(results[is.num], round, 3)
results

#-------------> Interaction effect

anova_data$intcond = as.factor(paste(anova_data$age2,anova_data$condition,sep = "_"))
as.data.frame(summarise(group_by(anova_data,intcond),M = round(mean(value),2),SD = round(sd(value),2)))

pwc <- anova_data  %>%
  pairwise_t_test(value ~ intcond, pool.sd = T,paired = T, var.equal = T,
                  comparisons = list(c("6_7_years_Treas_match_rate","6_7_years_Treas_nonmatch_rate"),
                                     c("Adults_Treas_match_rate","Adults_Treas_nonmatch_rate")),
                  p.adjust.method = "bonferroni")
pwc

CD =as.data.frame(cohens_d(anova_data,value~intcond, var.equal = T,paired = T,
                           comparisons = list(c("6_7_years_Treas_match_rate","6_7_years_Treas_nonmatch_rate"),
                                              c("Adults_Treas_match_rate","Adults_Treas_nonmatch_rate"))))
CD$effsize = round(CD$effsize,2)
CD


# All Pairwise

emmeans_test(anova_data,value~intcond,p.adjust.method = "bonferroni")

Inds = anova_data$intcond %in% c("6_7_years_Treas_match_rate","Adults_Treas_match_rate")
CD = cohen.d(anova_data$value[Inds],anova_data$intcond[Inds])
round(CD$cohen.d,2)



data$value = data$Treas_nonmatch_rate - data$Treas_match_rate
t.test(data$value[data$age2=="6_7_years"],data$value[data$age2=="Adults"],var.equal = T,paired = F)
summarise(group_by(data,age2),M = round(mean(value),2),SD = round(sd(value),2))

CD =as.data.frame(cohens_d(data,value~age2, var.equal = T,paired = F,
                           comparisons = list(c("6_7_years","Adults"))))
CD$effsize = round(CD$effsize,2)
CD

t.test(data$value[data$age2=="6_7_years"])
t.test(data$value[data$age2=="Adults"])
as.data.frame(cohens_d(data[data$age2=="6_7_years",],value~0,var.equal = T))
as.data.frame(cohens_d(data[data$age2=="Adults",],value~0,var.equal = T))

as.data.frame(cohens_d(anova_data[anova_data$age2=="6_7_years",],value~condition,var.equal = T,paired = T))


############################################# Expriment1 Figure stars - Accuracy------
data=subset(d[which(d$group=="exp1"),], select=c(subj, match_pc, nonmatch_pc, Gender, age2))

anova_data=melt(data, id.vars = c("Gender", "age2","subj"), 
                variable.name = "condition")
Conds = levels(anova_data$condition)

anova_data$intcond = as.factor(paste(anova_data$age2,anova_data$condition,sep = "_"))

as.data.frame(summarise(group_by(anova_data,intcond),M = round(mean(value),2),SD = round(sd(value),2)))

# Pairwise Comparisons:
pwc <- anova_data  %>%
  pairwise_t_test(value ~ intcond, pool.sd = T,paired = T,
                  comparisons = list(c(paste("6_7_years",Conds[1],sep = "_"),paste("6_7_years",Conds[2],sep = "_")),
                                     c(paste("9_10_years",Conds[1],sep = "_"),paste("9_10_years",Conds[2],sep = "_")),
                                     c(paste("Adults",Conds[1],sep = "_"),paste("Adults",Conds[2],sep = "_"))),
                  p.adjust.method = "bonferroni")
pwc
plotAcc1 =  ggplot(anova_data,aes(x=age2,y=value,fill=condition)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
  coord_cartesian(ylim = c(0.25,1))+
  theme_bw(base_family = "serif")+
  theme(strip.text.x = element_text(size = 8, face="bold"))+
  #scale_x_discrete(labels=c("OC", "YC"))+
  scale_fill_grey(labels=c("Match", "NonMatch"), start = 0.3, end = 0.8, na.value = "red", aesthetics = "fill")+
  # scale_fill_discrete(labels=c("Match", "NonMatch"),)+
  #ggtitle(tp_name)+
  labs(x=" ",y="Percent correct", size = 8)+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_text(size = 8,vjust=-0.25))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.text.x = element_text(size = 8)) + 
  theme(axis.text.y = element_text(size = 8))+
  theme(plot.title = element_text(size = 8, face="bold"))+
  theme(legend.text=element_text(size = 8))+
  ggtitle("Accuracy")+
  theme(plot.title = element_text(hjust = 0.5))+
  # theme(legend.title = element_text(colour="black", size = 8, face="bold"))+
  theme(legend.position="top",legend.title=element_blank(),legend.key.height=unit(.5,"line"))+
  graph_defaults+
  scale_y_continuous(expand = c(0,0),breaks = seq(0, 1, len = 5))
plot(plotAcc1)
############################################# Expriment1 Figure stars - Remember------
data=subset(d[which(d$group=="exp1"),], select=c(subj, R_match_rate, R_nonmatch_rate, Gender, age2))

anova_data=melt(data, id.vars = c("Gender", "age2","subj"), 
                variable.name = "condition")
Conds = levels(anova_data$condition)
anova_data$intcond = as.factor(paste(anova_data$age2,anova_data$condition,sep = "_"))
as.data.frame(summarise(group_by(anova_data,intcond),M = round(mean(value),2),SD = round(sd(value),2)))
# Pairwise Comparisons:
pwc <- anova_data  %>%
  pairwise_t_test(value ~ intcond, pool.sd = T,paired = T,
                  comparisons = list(c(paste("6_7_years",Conds[1],sep = "_"),paste("6_7_years",Conds[2],sep = "_")),
                                     c(paste("9_10_years",Conds[1],sep = "_"),paste("9_10_years",Conds[2],sep = "_")),
                                     c(paste("Adults",Conds[1],sep = "_"),paste("Adults",Conds[2],sep = "_"))),
                  p.adjust.method = "bonferroni")
pwc
plotRem1 =  ggplot(anova_data,aes(x=age2,y=value,fill=condition)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
  coord_cartesian(ylim = c(0.25,1))+
  theme_bw(base_family = "serif")+
  theme(strip.text.x = element_text(size = 8, face="bold"))+
  #scale_x_discrete(labels=c("OC", "YC"))+
  scale_fill_grey(labels=c("Match", "NonMatch"), start = 0.3, end = 0.8, na.value = "red", aesthetics = "fill")+
  # scale_fill_discrete(labels=c("Match", "NonMatch"),)+
  #ggtitle(tp_name)+
  labs(x=" ",y="Percent correct", size = 8)+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_text(size = 8,vjust=-0.25))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.text.x = element_text(size = 8)) + 
  theme(axis.text.y = element_text(size = 8))+
  theme(plot.title = element_text(size = 8, face="bold"))+
  theme(legend.text=element_text(size = 8))+
  ggtitle("Accuracy")+
  theme(plot.title = element_text(hjust = 0.5))+
  # theme(legend.title = element_text(colour="black", size = 8, face="bold"))+
  theme(legend.position="top",legend.title=element_blank(),legend.key.height=unit(.5,"line"))+
  graph_defaults+
  scale_y_continuous(expand = c(0,0),breaks = seq(0, 1, len = 5))
plot(plotRem1)
############################################# Expriment1 Figure stars - Treasure------
data=subset(d[which(d$group=="exp1"),], select=c(subj, Treas_match_rate, Treas_nonmatch_rate, Gender, age2))

anova_data=melt(data, id.vars = c("Gender", "age2","subj"), 
                variable.name = "condition")
Conds = levels(anova_data$condition)
anova_data$intcond = as.factor(paste(anova_data$age2,anova_data$condition,sep = "_"))
as.data.frame(summarise(group_by(anova_data,intcond),M = round(mean(value),2),SD = round(sd(value),2)))
# Pairwise Comparisons:
pwc <- anova_data  %>%
  pairwise_t_test(value ~ intcond, pool.sd = T,paired = T,
                  comparisons = list(c(paste("6_7_years",Conds[1],sep = "_"),paste("6_7_years",Conds[2],sep = "_")),
                                     c(paste("9_10_years",Conds[1],sep = "_"),paste("9_10_years",Conds[2],sep = "_")),
                                     c(paste("Adults",Conds[1],sep = "_"),paste("Adults",Conds[2],sep = "_"))),
                  p.adjust.method = "bonferroni")
pwc
plotTres1 =  ggplot(anova_data,aes(x=age2,y=value,fill=condition)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
  coord_cartesian(ylim = c(0.25,1))+
  theme_bw(base_family = "serif")+
  theme(strip.text.x = element_text(size = 8, face="bold"))+
  #scale_x_discrete(labels=c("OC", "YC"))+
  scale_fill_grey(labels=c("Match", "NonMatch"), start = 0.3, end = 0.8, na.value = "red", aesthetics = "fill")+
  # scale_fill_discrete(labels=c("Match", "NonMatch"),)+
  #ggtitle(tp_name)+
  labs(x=" ",y="Percent correct", size = 8)+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_text(size = 8,vjust=-0.25))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.text.x = element_text(size = 8)) + 
  theme(axis.text.y = element_text(size = 8))+
  theme(plot.title = element_text(size = 8, face="bold"))+
  theme(legend.text=element_text(size = 8))+
  ggtitle("Accuracy")+
  theme(plot.title = element_text(hjust = 0.5))+
  # theme(legend.title = element_text(colour="black", size = 8, face="bold"))+
  theme(legend.position="top",legend.title=element_blank(),legend.key.height=unit(.5,"line"))+
  graph_defaults+
  scale_y_continuous(expand = c(0,0),breaks = seq(0, 1, len = 5))
plot(plotTres1)


############################################# Expriment2 Figure stars - Accuracy------
data=subset(d[which(d$group=="exp2"),], select=c(subj, match_pc, nonmatch_pc, Gender, age2))

anova_data=melt(data, id.vars = c("Gender", "age2","subj"), 
                variable.name = "condition")
Conds = levels(anova_data$condition)

anova_data$intcond = as.factor(paste(anova_data$age2,anova_data$condition,sep = "_"))

as.data.frame(summarise(group_by(anova_data,intcond),M = round(mean(value),2),SD = round(sd(value),2)))

# Pairwise Comparisons:
pwc <- anova_data  %>%
  pairwise_t_test(value ~ intcond, pool.sd = T,paired = T,
                  comparisons = list(c(paste("6_7_years",Conds[1],sep = "_"),paste("6_7_years",Conds[2],sep = "_")),
                                     c(paste("Adults",Conds[1],sep = "_"),paste("Adults",Conds[2],sep = "_"))),
                  p.adjust.method = "bonferroni")
pwc
plotAcc2 =  ggplot(anova_data,aes(x=age2,y=value,fill=condition)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
  coord_cartesian(ylim = c(0.25,1))+
  theme_bw(base_family = "serif")+
  theme(strip.text.x = element_text(size = 8, face="bold"))+
  #scale_x_discrete(labels=c("OC", "YC"))+
  scale_fill_grey(labels=c("Match", "NonMatch"), start = 0.3, end = 0.8, na.value = "red", aesthetics = "fill")+
  # scale_fill_discrete(labels=c("Match", "NonMatch"),)+
  #ggtitle(tp_name)+
  labs(x=" ",y="Percent correct", size = 8)+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_text(size = 8,vjust=-0.25))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.text.x = element_text(size = 8)) + 
  theme(axis.text.y = element_text(size = 8))+
  theme(plot.title = element_text(size = 8, face="bold"))+
  theme(legend.text=element_text(size = 8))+
  ggtitle("Accuracy")+
  theme(plot.title = element_text(hjust = 0.5))+
  # theme(legend.title = element_text(colour="black", size = 8, face="bold"))+
  theme(legend.position="top",legend.title=element_blank(),legend.key.height=unit(.5,"line"))+
  graph_defaults+
  scale_y_continuous(expand = c(0,0),breaks = seq(0, 1, len = 5))
plot(plotAcc2)
############################################# Expriment2 Figure stars - Remember------
data=subset(d[which(d$group=="exp2"),], select=c(subj, R_match_rate, R_nonmatch_rate, Gender, age2))

anova_data=melt(data, id.vars = c("Gender", "age2","subj"), 
                variable.name = "condition")
Conds = levels(anova_data$condition)
anova_data$intcond = as.factor(paste(anova_data$age2,anova_data$condition,sep = "_"))
as.data.frame(summarise(group_by(anova_data,intcond),M = round(mean(value),2),SD = round(sd(value),2)))
# Pairwise Comparisons:
pwc <- anova_data  %>%
  pairwise_t_test(value ~ intcond, pool.sd = T,paired = T,
                  comparisons = list(c(paste("6_7_years",Conds[1],sep = "_"),paste("6_7_years",Conds[2],sep = "_")),
                                     c(paste("Adults",Conds[1],sep = "_"),paste("Adults",Conds[2],sep = "_"))),
                  p.adjust.method = "bonferroni")
pwc
plotRem2 =  ggplot(anova_data,aes(x=age2,y=value,fill=condition)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
  coord_cartesian(ylim = c(0.25,1))+
  theme_bw(base_family = "serif")+
  theme(strip.text.x = element_text(size = 8, face="bold"))+
  #scale_x_discrete(labels=c("OC", "YC"))+
  scale_fill_grey(labels=c("Match", "NonMatch"), start = 0.3, end = 0.8, na.value = "red", aesthetics = "fill")+
  # scale_fill_discrete(labels=c("Match", "NonMatch"),)+
  #ggtitle(tp_name)+
  labs(x=" ",y="Percent correct", size = 8)+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_text(size = 8,vjust=-0.25))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.text.x = element_text(size = 8)) + 
  theme(axis.text.y = element_text(size = 8))+
  theme(plot.title = element_text(size = 8, face="bold"))+
  theme(legend.text=element_text(size = 8))+
  ggtitle("Accuracy")+
  theme(plot.title = element_text(hjust = 0.5))+
  # theme(legend.title = element_text(colour="black", size = 8, face="bold"))+
  theme(legend.position="top",legend.title=element_blank(),legend.key.height=unit(.5,"line"))+
  graph_defaults+
  scale_y_continuous(expand = c(0,0),breaks = seq(0, 1, len = 5))
plot(plotRem2)
############################################# Expriment2 Figure stars - Treasure------
data=subset(d[which(d$group=="exp2"),], select=c(subj, Treas_match_rate, Treas_nonmatch_rate, Gender, age2))

anova_data=melt(data, id.vars = c("Gender", "age2","subj"), 
                variable.name = "condition")
Conds = levels(anova_data$condition)
anova_data$intcond = as.factor(paste(anova_data$age2,anova_data$condition,sep = "_"))
as.data.frame(summarise(group_by(anova_data,intcond),M = round(mean(value),2),SD = round(sd(value),2)))
# Pairwise Comparisons:
pwc <- anova_data  %>%
  pairwise_t_test(value ~ intcond, pool.sd = T,paired = T,
                  comparisons = list(c(paste("6_7_years",Conds[1],sep = "_"),paste("6_7_years",Conds[2],sep = "_")),
                                     c(paste("Adults",Conds[1],sep = "_"),paste("Adults",Conds[2],sep = "_"))),
                  p.adjust.method = "bonferroni")
pwc
plotTres2 =  ggplot(anova_data,aes(x=age2,y=value,fill=condition)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
  coord_cartesian(ylim = c(0.25,1))+
  theme_bw(base_family = "serif")+
  theme(strip.text.x = element_text(size = 8, face="bold"))+
  #scale_x_discrete(labels=c("OC", "YC"))+
  scale_fill_grey(labels=c("Match", "NonMatch"), start = 0.3, end = 0.8, na.value = "red", aesthetics = "fill")+
  # scale_fill_discrete(labels=c("Match", "NonMatch"),)+
  #ggtitle(tp_name)+
  labs(x=" ",y="Percent correct", size = 8)+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_text(size = 8,vjust=-0.25))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.text.x = element_text(size = 8)) + 
  theme(axis.text.y = element_text(size = 8))+
  theme(plot.title = element_text(size = 8, face="bold"))+
  theme(legend.text=element_text(size = 8))+
  ggtitle("Accuracy")+
  theme(plot.title = element_text(hjust = 0.5))+
  # theme(legend.title = element_text(colour="black", size = 8, face="bold"))+
  theme(legend.position="top",legend.title=element_blank(),legend.key.height=unit(.5,"line"))+
  graph_defaults+
  scale_y_continuous(expand = c(0,0),breaks = seq(0, 1, len = 5))
plot(plotTres2)

############################################# Expriment3 Figure stars - Accuracy------
data=subset(d[which(d$group=="exp3"),], select=c(subj, match_pc, nonmatch_pc, Gender, age2))

anova_data=melt(data, id.vars = c("Gender", "age2","subj"), 
                variable.name = "condition")
Conds = levels(anova_data$condition)

anova_data$intcond = as.factor(paste(anova_data$age2,anova_data$condition,sep = "_"))

as.data.frame(summarise(group_by(anova_data,intcond),M = round(mean(value),2),SD = round(sd(value),2)))

# Pairwise Comparisons:
pwc <- anova_data  %>%
  pairwise_t_test(value ~ intcond, pool.sd = T,paired = T,
                  comparisons = list(c(paste("6_7_years",Conds[1],sep = "_"),paste("6_7_years",Conds[2],sep = "_")),
                                     c(paste("Adults",Conds[1],sep = "_"),paste("Adults",Conds[2],sep = "_"))),
                  p.adjust.method = "bonferroni")
pwc
plotAcc3 =  ggplot(anova_data,aes(x=age2,y=value,fill=condition)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
  coord_cartesian(ylim = c(0.25,1))+
  theme_bw(base_family = "serif")+
  theme(strip.text.x = element_text(size = 8, face="bold"))+
  #scale_x_discrete(labels=c("OC", "YC"))+
  scale_fill_grey(labels=c("Match", "NonMatch"), start = 0.3, end = 0.8, na.value = "red", aesthetics = "fill")+
  # scale_fill_discrete(labels=c("Match", "NonMatch"),)+
  #ggtitle(tp_name)+
  labs(x=" ",y="Percent correct", size = 8)+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_text(size = 8,vjust=-0.25))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.text.x = element_text(size = 8)) + 
  theme(axis.text.y = element_text(size = 8))+
  theme(plot.title = element_text(size = 8, face="bold"))+
  theme(legend.text=element_text(size = 8))+
  ggtitle("Accuracy")+
  theme(plot.title = element_text(hjust = 0.5))+
  # theme(legend.title = element_text(colour="black", size = 8, face="bold"))+
  theme(legend.position="top",legend.title=element_blank(),legend.key.height=unit(.5,"line"))+
  graph_defaults+
  scale_y_continuous(expand = c(0,0),breaks = seq(0, 1, len = 5))
plot(plotAcc3)
############################################# Expriment3 Figure stars - Remember------
data=subset(d[which(d$group=="exp3"),], select=c(subj, R_match_rate, R_nonmatch_rate, Gender, age2))

anova_data=melt(data, id.vars = c("Gender", "age2","subj"), 
                variable.name = "condition")
Conds = levels(anova_data$condition)
anova_data$intcond = as.factor(paste(anova_data$age2,anova_data$condition,sep = "_"))
as.data.frame(summarise(group_by(anova_data,intcond),M = round(mean(value),2),SD = round(sd(value),2)))
# Pairwise Comparisons:
pwc <- anova_data  %>%
  pairwise_t_test(value ~ intcond, pool.sd = T,paired = T,
                  comparisons = list(c(paste("6_7_years",Conds[1],sep = "_"),paste("6_7_years",Conds[2],sep = "_")),
                                     c(paste("Adults",Conds[1],sep = "_"),paste("Adults",Conds[2],sep = "_"))),
                  p.adjust.method = "bonferroni")
pwc
plotRem3 =  ggplot(anova_data,aes(x=age2,y=value,fill=condition)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
  coord_cartesian(ylim = c(0.25,1))+
  theme_bw(base_family = "serif")+
  theme(strip.text.x = element_text(size = 8, face="bold"))+
  #scale_x_discrete(labels=c("OC", "YC"))+
  scale_fill_grey(labels=c("Match", "NonMatch"), start = 0.3, end = 0.8, na.value = "red", aesthetics = "fill")+
  # scale_fill_discrete(labels=c("Match", "NonMatch"),)+
  #ggtitle(tp_name)+
  labs(x=" ",y="Percent correct", size = 8)+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_text(size = 8,vjust=-0.25))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.text.x = element_text(size = 8)) + 
  theme(axis.text.y = element_text(size = 8))+
  theme(plot.title = element_text(size = 8, face="bold"))+
  theme(legend.text=element_text(size = 8))+
  ggtitle("Accuracy")+
  theme(plot.title = element_text(hjust = 0.5))+
  # theme(legend.title = element_text(colour="black", size = 8, face="bold"))+
  theme(legend.position="top",legend.title=element_blank(),legend.key.height=unit(.5,"line"))+
  graph_defaults+
  scale_y_continuous(expand = c(0,0),breaks = seq(0, 1, len = 5))
plot(plotRem3)
############################################# Expriment3 Figure stars - Treasure------
data=subset(d[which(d$group=="exp3"),], select=c(subj, Treas_match_rate, Treas_nonmatch_rate, Gender, age2))

anova_data=melt(data, id.vars = c("Gender", "age2","subj"), 
                variable.name = "condition")
Conds = levels(anova_data$condition)
anova_data$intcond = as.factor(paste(anova_data$age2,anova_data$condition,sep = "_"))
as.data.frame(summarise(group_by(anova_data,intcond),M = round(mean(value),2),SD = round(sd(value),2)))
# Pairwise Comparisons:
pwc <- anova_data  %>%
  pairwise_t_test(value ~ intcond, pool.sd = T,paired = T,
                  comparisons = list(c(paste("6_7_years",Conds[1],sep = "_"),paste("6_7_years",Conds[2],sep = "_")),
                                     c(paste("Adults",Conds[1],sep = "_"),paste("Adults",Conds[2],sep = "_"))),
                  p.adjust.method = "bonferroni")
pwc
plotTres3 =  ggplot(anova_data,aes(x=age2,y=value,fill=condition)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
  coord_cartesian(ylim = c(0.25,1))+
  theme_bw(base_family = "serif")+
  theme(strip.text.x = element_text(size = 8, face="bold"))+
  #scale_x_discrete(labels=c("OC", "YC"))+
  scale_fill_grey(labels=c("Match", "NonMatch"), start = 0.3, end = 0.8, na.value = "red", aesthetics = "fill")+
  # scale_fill_discrete(labels=c("Match", "NonMatch"),)+
  #ggtitle(tp_name)+
  labs(x=" ",y="Percent correct", size = 8)+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_text(size = 8,vjust=-0.25))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.text.x = element_text(size = 8)) + 
  theme(axis.text.y = element_text(size = 8))+
  theme(plot.title = element_text(size = 8, face="bold"))+
  theme(legend.text=element_text(size = 8))+
  ggtitle("Accuracy")+
  theme(plot.title = element_text(hjust = 0.5))+
  # theme(legend.title = element_text(colour="black", size = 8, face="bold"))+
  theme(legend.position="top",legend.title=element_blank(),legend.key.height=unit(.5,"line"))+
  graph_defaults+
  scale_y_continuous(expand = c(0,0),breaks = seq(0, 1, len = 5))
plot(plotTres3)


# Writing Out Graphs ------------------------------------------------------

mydoc = read_pptx();

mydoc = add_slide(mydoc, layout = "Title and Content",master = "Office Theme")
myPlt = dml(code = plot(plotAcc1))
mydoc = ph_with(mydoc,value = myPlt,location = ph_location(left = 1,top = 2, width = 3, height = 5 ))

mydoc = add_slide(mydoc, layout = "Title and Content",master = "Office Theme")
myPlt = dml(code = plot(plotRem1))
mydoc = ph_with(mydoc,value = myPlt,location = ph_location(left = 1,top = 2, width = 3, height = 5 ))

mydoc = add_slide(mydoc, layout = "Title and Content",master = "Office Theme")
myPlt = dml(code = plot(plotTres1))
mydoc = ph_with(mydoc,value = myPlt,location = ph_location(left = 1,top = 2, width = 3, height = 5 ))

mydoc = add_slide(mydoc, layout = "Title and Content",master = "Office Theme")
myPlt = dml(code = plot(plotAcc2))
mydoc = ph_with(mydoc,value = myPlt,location = ph_location(left = 1,top = 2, width = 3, height = 5 ))

mydoc = add_slide(mydoc, layout = "Title and Content",master = "Office Theme")
myPlt = dml(code = plot(plotRem2))
mydoc = ph_with(mydoc,value = myPlt,location = ph_location(left = 1,top = 2, width = 3, height = 5 ))

mydoc = add_slide(mydoc, layout = "Title and Content",master = "Office Theme")
myPlt = dml(code = plot(plotTres2))
mydoc = ph_with(mydoc,value = myPlt,location = ph_location(left = 1,top = 2, width = 3, height = 5 ))

mydoc = add_slide(mydoc, layout = "Title and Content",master = "Office Theme")
myPlt = dml(code = plot(plotAcc3))
mydoc = ph_with(mydoc,value = myPlt,location = ph_location(left = 1,top = 2, width = 3, height = 5 ))

mydoc = add_slide(mydoc, layout = "Title and Content",master = "Office Theme")
myPlt = dml(code = plot(plotRem3))
mydoc = ph_with(mydoc,value = myPlt,location = ph_location(left = 1,top = 2, width = 3, height = 5 ))

mydoc = add_slide(mydoc, layout = "Title and Content",master = "Office Theme")
myPlt = dml(code = plot(plotTres3))
mydoc = ph_with(mydoc,value = myPlt,location = ph_location(left = 1,top = 2, width = 3, height = 5 ))

print(mydoc, target = "R_Plot_AK.pptx" )