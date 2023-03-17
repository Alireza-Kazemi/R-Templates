rm(list=ls())
x=readline()
C:\Users\asus\Dropbox\Metacog_Illusion
setwd(x)
getwd()


library(export)
graph2ppt(file="Figagreetohint1.pptx",width = 5, height = 5)

library(pacman)
p_load(reshape2,ez,lme4,lmerTest,
       ggplot2,grid,tidyr,Rmisc,dplyr,
       effects,gridExtra,psych,Cairo,
       corrplot,knitr,
       PerformanceAnalytics,officer,rvg)


graph_defaults=theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_text(size = 18,vjust=-0.25))+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.text.x = element_text(size = 16)) + 
  theme(axis.text.y = element_text(size = 16))+
  theme(plot.title = element_text(size = 18, face="bold"))+
  theme(legend.text=element_text(size=14))+
  #theme(legend.title = element_text(colour="black", size=14, face="bold"))+
  #theme(legend.position="right",legend.title=element_blank(),legend.key.height=unit(.5,"line"))+
  theme(strip.text.x = element_text(size=18, face="bold"),
        strip.text.y = element_text(size=18, face="bold"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.background = element_rect(fill = "transparent",colour = NA))+
  theme(plot.background = element_rect(fill = "transparent",colour = NA))+
  theme(strip.background = element_rect(fill="transparent"))
  # scale_fill_grey()+
  # theme(panel.grid = element_blank(),
  #       panel.border = element_blank())


# Read Exp 1 Data ---------------------------------------------------------
Path = paste("C:/Users/asus/Dropbox/Metacog_Illusion")
Path = paste(Path,"/Exp1/",sep = "")

d=read.table(paste(Path,'all_data_subjremoved.xls',sep = ""), header=TRUE, sep="\t", strip.white = TRUE)


d$age2=factor(d$age2, levels = c("YC", "OC", "adults"))


###############################  First Graphs ----
dat = d[,c("subj","age2","match_pc","nonmatch_pc")]
anova_data=melt(dat, id.vars = c("age2","subj"), 
                measure.vars = c('match_pc', 'nonmatch_pc'), 
                variable.name = "condition")
summarise(group_by(anova_data,age2),mean(value),sd(value))
plotAcc1 =  ggplot(anova_data,aes(x=age2,y=value,fill=condition)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
  coord_cartesian(ylim = c(0.2,1))+
  theme_bw(base_family = "serif")+
  theme(strip.text.x = element_text(size=16, face="bold"))+
  #scale_x_discrete(labels=c("OC", "YC"))+
  scale_fill_grey(labels=c("Match", "NonMatch"), start = 0.3, end = 0.8, na.value = "red", aesthetics = "fill")+
  # scale_fill_discrete(labels=c("Match", "NonMatch"),)+
  #ggtitle(tp_name)+
  labs(x=" ",y="Percent correct", size=16)+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_text(size = 18,vjust=-0.25))+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.text.x = element_text(size = 16)) + 
  theme(axis.text.y = element_text(size = 16))+
  theme(plot.title = element_text(size = 18, face="bold"))+
  theme(legend.text=element_text(size=14))+
  ggtitle("Accuracy")+
  theme(plot.title = element_text(hjust = 0.5))+
  # theme(legend.title = element_text(colour="black", size=14, face="bold"))+
  theme(legend.position="top",legend.title=element_blank(),legend.key.height=unit(.5,"line"))+
  graph_defaults+
  scale_y_continuous(expand = c(0,0))
plot(plotAcc1)


dat = d[,c("subj","age2","R_match_rate","R_nonmatch_rate")]
anova_data=melt(dat, id.vars = c("age2","subj"), 
                measure.vars = c('R_match_rate', 'R_nonmatch_rate'), 
                variable.name = "condition")
summarise(group_by(anova_data,age2),mean(value),sd(value))

plotRem1 =  ggplot(anova_data,aes(x=age2,y=value,fill=condition)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
  coord_cartesian(ylim = c(0.2,1))+
  theme_bw(base_family = "serif")+
  theme(strip.text.x = element_text(size=16, face="bold"))+
  #scale_x_discrete(labels=c("OC", "YC"))+
  scale_fill_grey(labels=c("Match", "NonMatch"), start = 0.3, end = 0.8, na.value = "red", aesthetics = "fill")+
  #ggtitle(tp_name)+
  labs(x=" ",y="Remember Rate", size=16)+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_text(size = 18,vjust=-0.25))+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.text.x = element_text(size = 16)) + 
  theme(axis.text.y = element_text(size = 16))+
  theme(plot.title = element_text(size = 18, face="bold"))+
  theme(legend.text=element_text(size=14))+
  ggtitle("Remember")+
  theme(plot.title = element_text(hjust = 0.5))+
  #theme(legend.title = element_text(colour="black", size=14, face="bold"))+
  theme(legend.position="top",legend.title=element_blank(),legend.key.height=unit(.5,"line"))+
  graph_defaults+   scale_y_continuous(expand = c(0,0))
plot(plotRem1)

dat = d[,c("subj","age2","Treas_match_rate","Treas_nonmatch_rate")]
anova_data=melt(dat, id.vars = c("age2","subj"), 
                measure.vars = c('Treas_match_rate', 'Treas_nonmatch_rate'), 
                variable.name = "condition")
summarise(group_by(anova_data,condition),mean(value),sd(value))

plotTres1 =  ggplot(anova_data,aes(x=age2,y=value,fill=condition)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
  coord_cartesian(ylim = c(0.2,1))+
  theme_bw(base_family = "serif")+
  theme(strip.text.x = element_text(size=16, face="bold"))+
  #scale_x_discrete(labels=c("OC", "YC"))+
  scale_fill_grey(labels=c("Match", "NonMatch"), start = 0.3, end = 0.8, na.value = "red", aesthetics = "fill")+
  #ggtitle(tp_name)+
  labs(x=" ",y="Treasure Rate", size=16)+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_text(size = 18,vjust=-0.25))+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.text.x = element_text(size = 16)) + 
  theme(axis.text.y = element_text(size = 16))+
  theme(plot.title = element_text(size = 18, face="bold"))+
  theme(legend.text=element_text(size=14))+
  ggtitle("Treasure")+
  theme(plot.title = element_text(hjust = 0.5))+
  #theme(legend.title = element_text(colour="black", size=14, face="bold"))+
  theme(legend.position="top",legend.title=element_blank(),legend.key.height=unit(.5,"line"))+
  graph_defaults+   scale_y_continuous(expand = c(0,0))
plot(plotTres1)



# Read Exp 2 Data ---------------------------------------------------------
Path = paste("C:/Users/asus/Dropbox/Metacog_Illusion")
Path = paste(Path,"/Exp2/",sep = "")

d=read.table(paste(Path,'alldata_relevant_subjremoved.xls',sep = ""), header=TRUE, sep="\t", strip.white = TRUE)


d$age2=factor(d$age2, levels = c("child", "adults"))

###############################  Second Graphs ----
dat = d[d$Group==2,c("subj","age2","match_pc","nonmatch_pc")]
anova_data=melt(dat, id.vars = c("age2","subj"), 
                measure.vars = c('match_pc', 'nonmatch_pc'), 
                variable.name = "condition")
summarise(group_by(anova_data,condition),mean(value),sd(value))
plotAcc2 =  ggplot(anova_data,aes(x=age2,y=value,fill=condition)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
  coord_cartesian(ylim = c(0.2,1))+
  theme_bw(base_family = "serif")+
  theme(strip.text.x = element_text(size=16, face="bold"))+
  #scale_x_discrete(labels=c("OC", "YC"))+
  scale_fill_grey(labels=c("Match", "NonMatch"), start = 0.3, end = 0.8, na.value = "red", aesthetics = "fill")+
  #ggtitle(tp_name)+
  labs(x=" ",y="Percent correct", size=16)+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_text(size = 18,vjust=-0.25))+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.text.x = element_text(size = 16)) + 
  theme(axis.text.y = element_text(size = 16))+
  theme(plot.title = element_text(size = 18, face="bold"))+
  theme(legend.text=element_text(size=14))+
  ggtitle("Accuracy")+
  theme(plot.title = element_text(hjust = 0.5))+
  #theme(legend.title = element_text(colour="black", size=14, face="bold"))+
  theme(legend.position="top",legend.title=element_blank(),legend.key.height=unit(.5,"line"))+
  graph_defaults+   scale_y_continuous(expand = c(0,0))
plot(plotAcc2)


dat = d[d$Group==2,c("subj","age2","R_match_rate","R_nonmatch_rate")]
anova_data=melt(dat, id.vars = c("age2","subj"), 
                measure.vars = c('R_match_rate', 'R_nonmatch_rate'), 
                variable.name = "condition")
summarise(group_by(anova_data,age2,condition),mean(value),sd(value))

plotRem2 =  ggplot(anova_data,aes(x=age2,y=value,fill=condition)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
  coord_cartesian(ylim = c(0.2,1))+
  theme_bw(base_family = "serif")+
  theme(strip.text.x = element_text(size=16, face="bold"))+
  #scale_x_discrete(labels=c("OC", "YC"))+
  scale_fill_grey(labels=c("Match", "NonMatch"), start = 0.3, end = 0.8, na.value = "red", aesthetics = "fill")+
  #ggtitle(tp_name)+
  labs(x=" ",y="Remember Rate", size=16)+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_text(size = 18,vjust=-0.25))+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.text.x = element_text(size = 16)) + 
  theme(axis.text.y = element_text(size = 16))+
  theme(plot.title = element_text(size = 18, face="bold"))+
  theme(legend.text=element_text(size=14))+
  ggtitle("Remember")+
  theme(plot.title = element_text(hjust = 0.5))+
  #theme(legend.title = element_text(colour="black", size=14, face="bold"))+
  theme(legend.position="top",legend.title=element_blank(),legend.key.height=unit(.5,"line"))+
  graph_defaults+   scale_y_continuous(expand = c(0,0))
plot(plotRem2)

dat = d[d$Group==2,c("subj","age2","Treas_match_rate","Treas_nonmatch_rate")]
anova_data=melt(dat, id.vars = c("age2","subj"), 
                measure.vars = c('Treas_match_rate', 'Treas_nonmatch_rate'), 
                variable.name = "condition")
summarise(group_by(anova_data,age2,condition),mean(value),sd(value))

plotTres2 =  ggplot(anova_data,aes(x=age2,y=value,fill=condition)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
  coord_cartesian(ylim = c(0.2,1))+
  theme_bw(base_family = "serif")+
  theme(strip.text.x = element_text(size=16, face="bold"))+
  #scale_x_discrete(labels=c("OC", "YC"))+
  scale_fill_grey(labels=c("Match", "NonMatch"), start = 0.3, end = 0.8, na.value = "red", aesthetics = "fill")+
  #ggtitle(tp_name)+
  labs(x=" ",y="Treasure Rate", size=16)+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_text(size = 18,vjust=-0.25))+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.text.x = element_text(size = 16)) + 
  theme(axis.text.y = element_text(size = 16))+
  theme(plot.title = element_text(size = 18, face="bold"))+
  theme(legend.text=element_text(size=14))+
  ggtitle("Treasure")+
  theme(plot.title = element_text(hjust = 0.5))+
  #theme(legend.title = element_text(colour="black", size=14, face="bold"))+
  theme(legend.position="top",legend.title=element_blank(),legend.key.height=unit(.5,"line"))+
  graph_defaults+   scale_y_continuous(expand = c(0,0))
plot(plotTres2)




###############################  Third Graphs ----
dat = d[d$Group==1,c("subj","age2","match_pc","nonmatch_pc")]
anova_data=melt(dat, id.vars = c("age2","subj"), 
                measure.vars = c('match_pc', 'nonmatch_pc'), 
                variable.name = "condition")
summarise(group_by(anova_data,condition),mean(value),sd(value))
plotAcc3 =  ggplot(anova_data,aes(x=age2,y=value,fill=condition)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
  coord_cartesian(ylim = c(0.2,1))+
  theme_bw(base_family = "serif")+
  theme(strip.text.x = element_text(size=16, face="bold"))+
  #scale_x_discrete(labels=c("OC", "YC"))+
  scale_fill_grey(labels=c("Match", "NonMatch"), start = 0.3, end = 0.8, na.value = "red", aesthetics = "fill")+
  #ggtitle(tp_name)+
  labs(x=" ",y="Percent correct", size=16)+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_text(size = 18,vjust=-0.25))+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.text.x = element_text(size = 16)) + 
  theme(axis.text.y = element_text(size = 16))+
  theme(plot.title = element_text(size = 18, face="bold"))+
  theme(legend.text=element_text(size=14))+
  ggtitle("Accuracy")+
  theme(plot.title = element_text(hjust = 0.5))+
  #theme(legend.title = element_text(colour="black", size=14, face="bold"))+
  theme(legend.position="top",legend.title=element_blank(),legend.key.height=unit(.5,"line"))+
  graph_defaults+   scale_y_continuous(expand = c(0,0))
plot(plotAcc3)


dat = d[d$Group==1,c("subj","age2","R_match_rate","R_nonmatch_rate")]
anova_data=melt(dat, id.vars = c("age2","subj"), 
                measure.vars = c('R_match_rate', 'R_nonmatch_rate'), 
                variable.name = "condition")
summarise(group_by(anova_data,age2,condition),mean(value),sd(value))

plotRem3 =  ggplot(anova_data,aes(x=age2,y=value,fill=condition)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
  coord_cartesian(ylim = c(0.2,1))+
  theme_bw(base_family = "serif")+
  theme(strip.text.x = element_text(size=16, face="bold"))+
  #scale_x_discrete(labels=c("OC", "YC"))+
  scale_fill_grey(labels=c("Match", "NonMatch"), start = 0.3, end = 0.8, na.value = "red", aesthetics = "fill")+
  #ggtitle(tp_name)+
  labs(x=" ",y="Remember Rate", size=16)+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_text(size = 18,vjust=-0.25))+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.text.x = element_text(size = 16)) + 
  theme(axis.text.y = element_text(size = 16))+
  theme(plot.title = element_text(size = 18, face="bold"))+
  theme(legend.text=element_text(size=14))+
  ggtitle("Remember")+
  theme(plot.title = element_text(hjust = 0.5))+
  #theme(legend.title = element_text(colour="black", size=14, face="bold"))+
  theme(legend.position="top",legend.title=element_blank(),legend.key.height=unit(.5,"line"))+
  graph_defaults+   scale_y_continuous(expand = c(0,0))
plot(plotRem3)

dat = d[d$Group==1,c("subj","age2","Treas_match_rate","Treas_nonmatch_rate")]
anova_data=melt(dat, id.vars = c("age2","subj"), 
                measure.vars = c('Treas_match_rate', 'Treas_nonmatch_rate'), 
                variable.name = "condition")
summarise(group_by(anova_data,age2,condition),mean(value),sd(value))

plotTres3 =  ggplot(anova_data,aes(x=age2,y=value,fill=condition)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
  coord_cartesian(ylim = c(0.2,1))+
  theme_bw(base_family = "serif")+
  theme(strip.text.x = element_text(size=16, face="bold"))+
  #scale_x_discrete(labels=c("OC", "YC"))+
  scale_fill_grey(labels=c("Match", "NonMatch"), start = 0.3, end = 0.8, na.value = "red", aesthetics = "fill")+
  #ggtitle(tp_name)+
  labs(x=" ",y="Treasure Rate", size=16)+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_text(size = 18,vjust=-0.25))+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.text.x = element_text(size = 16)) + 
  theme(axis.text.y = element_text(size = 16))+
  theme(plot.title = element_text(size = 18, face="bold"))+
  theme(legend.text=element_text(size=14))+
  ggtitle("Treasure")+
  theme(plot.title = element_text(hjust = 0.5))+
  #theme(legend.title = element_text(colour="black", size=14, face="bold"))+
  theme(legend.position="top",legend.title=element_blank(),legend.key.height=unit(.5,"line"))+
  graph_defaults+   scale_y_continuous(expand = c(0,0))
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

print(mydoc, target = "R_Plot_AK_test.pptx" )


# mydoc = read_pptx();
# mydoc = add_slide(mydoc, layout = "Title and Content",master = "Office Theme")
# mydoc = ph_with_vg_at(mydoc,code = print(plotAcc1),left = 1,top = 2, width = 3, height = 5 )
# mydoc = add_slide(mydoc, layout = "Title and Content",master = "Office Theme")
# mydoc = ph_with_vg_at(mydoc,code = print(plotRem1), left = 1,top = 2, width = 3, height = 5  )
# mydoc = add_slide(mydoc, layout = "Title and Content",master = "Office Theme")
# mydoc = ph_with_vg_at(mydoc,code = print(plotTres1), left = 1,top = 2, width = 3, height = 5   )
# mydoc = add_slide(mydoc, layout = "Title and Content",master = "Office Theme")
# mydoc = ph_with_vg_at(mydoc,code = print(plotAcc2),left = 1,top = 2, width = 3, height = 5 )
# mydoc = add_slide(mydoc, layout = "Title and Content",master = "Office Theme")
# mydoc = ph_with_vg_at(mydoc,code = print(plotRem2),left = 1,top = 2, width = 3, height = 5 )
# mydoc = add_slide(mydoc, layout = "Title and Content",master = "Office Theme")
# mydoc = ph_with_vg_at(mydoc,code = print(plotTres2),left = 1,top = 2, width = 3, height = 5  )
# mydoc = add_slide(mydoc, layout = "Title and Content",master = "Office Theme")
# mydoc = ph_with_vg_at(mydoc,code = print(plotAcc3),left = 1,top = 2, width = 3, height = 5   )
# mydoc = add_slide(mydoc, layout = "Title and Content",master = "Office Theme")
# mydoc = ph_with_vg_at(mydoc,code = print(plotRem3),left = 1,top = 2, width = 3, height = 5 )
# mydoc = add_slide(mydoc, layout = "Title and Content",master = "Office Theme")
# mydoc = ph_with_vg_at(mydoc,code = print(plotTres3),left = 1,top = 2, width = 3, height = 5  )
