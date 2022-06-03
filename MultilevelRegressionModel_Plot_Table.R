library(pacman)
p_load(reshape2,
       ez,
       lme4,
       lmerTest,
       ggplot2,
       grid,
       tidyr,
       plyr,
       dplyr,
       effects,
       gridExtra,
       DescTools,
       Cairo, #alternate image writing package with superior performance.
       corrplot,
       knitr,
       PerformanceAnalytics,
       afex,
       ggpubr,
       readxl,
       officer,
       psych,
       rstatix,
       emmeans,
       broom,
       ordinal,
       sjPlot)


# http://www.strengejacke.de/sjPlot/index.html

############ Experiment1 hint-asked Group = 2 Help-seeking and recognition memory decisions -------------
dat = read.table("testtrialdata_subjrem.csv",header=TRUE,sep=',')
dat = dat[dat$group==2,]  
dat = dat[dat$hint_status=="Used",] # only cued trials that hint is used
dat$age = factor(dat$age,levels = c("5","7","9"))

dat = dat[,c("subj","age","agreed")]
dat$Followed =case_when(dat$agreed == "yes" ~1,
                        dat$agreed == "no" ~0)


m1<-glmer(Followed~age+(1|subj),data=dat,family = binomial,na.action=na.exclude)
summary(m1)

plot(allEffects(m1, default.levels=4)) 

A = allEffects(m1)
plot(A) 
tab_model(m1,show.se = T,show.stat = T,show.est = T)
summary(A$age)
A$age$se


