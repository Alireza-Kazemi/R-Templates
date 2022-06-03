p1 = ggplot(temp,aes(x=time , y=value, fill = condition)) +
  # geom_bar(stat="summary",fun="mean",position="dodge")+
  geom_jitter(position = position_jitterdodge(jitter.width = NULL,
                                              jitter.height = 0,
                                              dodge.width = .75),shape = 21,fill="grey",aes(colour = condition))+
  # geom_boxplot(outlier.colour="black",outlier.shape=8,
  #              size=.5,fill = NA,aes(colour = condition))+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge",size=1,
               aes(colour = condition))+
  theme_bw(base_family = "serif")+
  facet_wrap(~variable)

plot(p1)  