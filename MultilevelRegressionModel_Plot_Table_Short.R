############ Experiment1 hint-asked Group = 2 Help-seeking and recognition memory decisions -------------
dat = read.table("testtrialdata_subjrem.csv",header=TRUE,sep=',')
dat = dat[dat$group==2,]  
dat = dat[dat$hint_status=="Used",] # only cued trials that hint is used
dat$age = factor(dat$age,levels = c("5","7","9"))

dat = dat[,c("subj","age","agreed")]
dat$agree =case_when(dat$agreed == "yes" ~1,
                        dat$agreed == "no" ~0)

m1<-glmer(agree~age+(1|subj),data=dat,family = binomial,na.action=na.exclude)
summary(m1)


A = allEffects(m1)
plot(A) 
tab_model(m1,show.se = T,show.stat = T,show.est = T)
summary(A$age)
A$age$se

# AgreetoHint = A$age$data
# AgreetoHint$Experiment = "Experiment 1"

# Fig1 = ggplot(AgreetoHint,aes(x=age, y=agree, fill = age)) + 
#   geom_bar(stat="summary",fun="mean",position="dodge")+
#   stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
#   theme_bw(base_family = "serif")+
#   theme(strip.text.x = element_text(size=16, face="bold"))+
#   labs(x="",y="Pattern Similarity", size=16)+
#   theme(axis.title.y = element_text(size = 18))+
#   theme(axis.text.x = element_text(size = 16))+
#   theme(strip.text.x = element_text(size=16, face="bold"),
#         strip.text.y = element_text(size=16, face="bold"))
# 
# plot(Fig1)


d2 = summarise(group_by(dat,age),M = mean(agree))
d1 = as.data.frame(effect("age",m1))
d2$age = as.numeric(as.character(d2$age))
d1$age = as.numeric(as.character(d1$age))

ggplot()+
  # geom_line(data=d2,aes(x = age, y = M),lwd=1, color= "blue",alpha=0.5)+
  # geom_point(data=d2,aes(x = age, y = M),size=3, color= "blue",alpha=0.5)+
  geom_line(data=d1,aes(age, fit),lwd=1, linetype="dotted", color= "blue")+
  geom_point(data=d1,aes(age, fit),size=2, color= "blue")+
  geom_errorbar(data=d1,aes(x=age, ymin=lower, ymax=upper), color= "blue",width = 0.05,size = .8)+
  theme_bw(base_family = "serif")+
  labs(x="Age",y="Agree", size=16)+
  theme(axis.title.y = element_text(size = 16))+
  theme(axis.title.x = element_text(size = 16))+
  theme(axis.text.x = element_text(size = 16))+
  theme(axis.text.y = element_text(size = 16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=c(5,7,9))+
  ylim(0.5,.95)+
  scale_fill_grey()

graph2ppt(file="Figagreetohint1.pptx",width = 5, height = 5)