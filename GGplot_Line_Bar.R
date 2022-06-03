


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
       export,
       officer,
       rvg,
       jmotif,ppcor,MASS,correlation)







ggplot(Graphdata,aes(x=RespType2,y=RT,fill=ConditionName)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")
 
 
ggplot(Sdat,aes(x=age_group, y=value, fill = condition)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
  # facet_wrap(.~sex)+
  theme_bw(base_family = "serif")+
  theme(strip.text.x = element_text(size=16, face="bold"))+
  labs(x="",y="Value", size=16)+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.text.x = element_text(size = 16))+
  ylim(0,1)+
  graph_defaults+
  scale_fill_grey()+
  theme(strip.text.x = element_text(size=16, face="bold"),
        strip.text.y = element_text(size=16, face="bold"))
		
 
 
 
Fig1 = ggplot(Sdat,aes(x=age_group, y=Corval, fill = condition)) + 
		geom_bar(stat="summary",fun="mean",position="dodge")+
		geom_jitter(position = position_jitterdodge(jitter.width = .4,
													jitter.height = 0,
													dodge.width = .9),shape = 21,fill="grey",aes(colour = condition))+
		stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
		facet_wrap(~ROI)+
		theme_bw(base_family = "serif")+
		theme(strip.text.x = element_text(size=16, face="bold"))+
		labs(x="",y="Pattern Similarity", size=16)+
		theme(axis.title.y = element_text(size = 18))+
		theme(axis.text.x = element_text(size = 16))+
		graph_defaults+
		#scale_fill_grey()+
		theme(strip.text.x = element_text(size=16, face="bold"),
			strip.text.y = element_text(size=16, face="bold"))

plot(Fig1)
graph2ppt(file="Fig1.pptx",width = 9, height = 5)


  
Graphdata=melt(dat, id.vars = c("SID", "RespType2","ConditionName","timeStamp"), 
		   measure.vars = c('FixOnContent', 'FixOnTarget', 'FixOnLure'), 
		   variable.name = "Gaze")

ggplot(Graphdata,aes(x=timeStamp,y=value,colour=Gaze)) +
  geom_line(stat="summary",fun="mean",position=position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_se", geom="errorbar",position = position_dodge(width = 0.9), alpha = 0.5)+
  facet_grid(RespType2~ConditionName)