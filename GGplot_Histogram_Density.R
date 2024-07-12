dat = datRet[datRet$timeStamp>500,]
dat = dat[dat$timeStamp<2000,]
dat = dat[!(is.na(dat$GazeX)|is.na(dat$GazeY)),]

radius = 50
decayFactor = 1
percent = .4
coefDecay = radius^decayFactor/log(percent)  # 10% at 50 pixel



dat$lureDist = sqrt((dat$GazeX_Shifted-(dat$X_CenterLure+ (1280-1)/2 - dat$X_Center))^2 + (dat$GazeY - dat$Y_Center)^2)
dat$targetDist = sqrt((dat$GazeX_Shifted-(dat$X_CenterTarget+ (1280-1)/2 - dat$X_Center))^2 + (dat$GazeY - dat$Y_Center)^2)
dat$Dist = case_when(dat$RespType2 == "Corr Loc"   ~ dat$targetDist,
                     dat$RespType2 == "Incorr Loc" ~ dat$lureDist,
                     dat$RespType2 == "Miss"   ~ dat$targetDist+dat$lureDist)



dat$feat_ItemDistance = exp(1/coefDecay*sqrt((dat$GazeX_Shifted-(dat$X_CenterTarget+ (1280-1)/2 - dat$X_Center))^2 + (dat$GazeY - dat$Y_Center)^2)^decayFactor)
dat$feat_LureDistance = exp(1/coefDecay*sqrt((dat$GazeX_Shifted-(dat$X_CenterLure+ (1280-1)/2 - dat$X_Center))^2 + (dat$GazeY - dat$Y_Center)^2)^decayFactor)

dat$feat_ItemDistance = dat$feat_ItemDistance * dat$CenterDiff
dat$feat_LureDistance = dat$feat_LureDistance * dat$CenterDiff

dat$feat_BorderDistance = exp(1/coefDecay*abs(dat$GazeX - 1)^decayFactor) +
                          exp(1/coefDecay*abs(dat$GazeX-1280)^decayFactor)


dat$Feat = case_when(dat$RespType2 == "Corr Loc"   ~ (dat$feat_ItemDistance+dat$feat_BorderDistance)/(dat$feat_BorderDistance+dat$feat_ItemDistance+dat$feat_LureDistance+0.1) ,
                     dat$RespType2 == "Incorr Loc" ~ (dat$feat_LureDistance)/(dat$feat_BorderDistance+dat$feat_ItemDistance+dat$feat_LureDistance+0.1) )


# --------------------------------------------- Graphing
graphDat = dat[(dat$RespType2 %in% c("Corr Loc","Incorr Loc","Miss")) &
                 dat$GazeX_Shifted>300 & dat$GazeX_Shifted<940,]
densityData = graphDat

ggplot(data = graphDat,aes(x = GazeX_Shifted,fill = RespType2,color = RespType2,group = RespType2))+
  geom_histogram(aes(y = after_stat(count/max(count))),
                 binwidth = sd(graphDat$GazeX_Shifted)/20,alpha = .25,
                 color = "gray",position = "identity")+
  geom_density(aes(y = after_stat(count/max(count))),
               bw = sd(graphDat$GazeX_Shifted)/20,linewidth = 1,alpha = 1,
               fill = NA,position = "identity") +
  facet_grid(ConditionName~Target)+
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_line(color = alpha("gray", alpha = 0.3)))


graphDat = dat[(dat$RespType2 %in% c("Corr Loc","Incorr Loc")) &
                 dat$GazeX_Shifted>200 & dat$GazeX_Shifted<1000,]
graphDat$GazeX_Shifted = round(graphDat$GazeX_Shifted)

graphDat = graphDat %>% group_by(SID,Target,ConditionName,RespType2,GazeX_Shifted,TrialNum) %>%
  summarise(Feat = mean(Feat,na.rm=T),N=n(),
            feat_ItemDistance = mean(feat_ItemDistance,na.rm=T),
            feat_LureDistance = mean(feat_LureDistance,na.rm=T)) %>%
  group_by(SID,Target,ConditionName,RespType2,GazeX_Shifted) %>%
  summarise(Feat = sum(Feat*N,na.rm=T)/sum(N),
            feat_ItemDistance = sum(feat_ItemDistance*N,na.rm=T)/sum(N),
            feat_LureDistance = sum(feat_LureDistance*N,na.rm=T)/sum(N))%>%as.data.frame()
lineDat = graphDat %>% group_by(Target,ConditionName,RespType2,GazeX_Shifted) %>%
  summarise(feat_ItemDistance = mean(feat_ItemDistance,na.rm=T),
            feat_LureDistance = mean(feat_LureDistance,na.rm=T)) %>% as.data.frame()
# --------------------------------------------- Graphing
ggplot(data = graphDat,aes(x = GazeX_Shifted,color = RespType2,group = RespType2))+
  geom_point(aes(y = Feat),shape=20)+
  geom_density(data = densityData,aes(y = after_stat(count/max(count))),
               bw = sd(graphDat$GazeX_Shifted)/20,linewidth = 1,alpha = 1,
               fill = NA,position = "identity") +
   facet_grid(ConditionName~Target)+
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_line(color = alpha("gray", alpha = 0.3)))



# --------------------------------------------- Graphing
graphDat = dat[(dat$RespType2 %in% c("Corr Loc","Incorr Loc")) &
                 abs(dat$Dist)<500,]

ggplot(data = graphDat,aes(x = Dist,color = RespType2,group = RespType2))+
  geom_histogram(aes(y = after_stat(count/max(count)),fill = RespType2),
                 binwidth = sd(graphDat$Dist)/20,alpha = .75,position = "identity")+
  geom_density(aes(y = after_stat(count/max(count))),
               bw = sd(graphDat$Dist)/20,linewidth = 1,alpha = 1,
               fill = NA,position = "identity") +
  facet_grid(ConditionName~Target+RespType2)

graphDat = dat[(dat$RespType2 %in% c("Corr Loc","Incorr Loc")) &
                 abs(dat$Dist)<500,]
graphDat$Dist = round(graphDat$Dist)

graphDat = graphDat %>% group_by(SID,Target,ConditionName,RespType2,Dist,TrialNum) %>%
  summarise(Feat = mean(Feat,na.rm=T),N=n()) %>%
  group_by(SID,Target,ConditionName,RespType2,Dist) %>%
  summarise(Feat = sum(Feat*N,na.rm=T)/sum(N),N= sum(N) )%>%as.data.frame()

ggplot(data = graphDat,aes(x = Dist,color = RespType2,group = RespType2))+
  geom_point(aes(y = Feat),shape=20)+
  geom_density(aes(y = after_stat(count/max(count))),
               bw = sd(graphDat$Dist)/15,linewidth = 1,alpha = 1,
               fill = NA,position = "identity") +
  facet_grid(ConditionName~Target+RespType2)



ggplot(data = graphDat,aes(x = GazeX_Shifted,fill = RespType2,color = RespType2,group = RespType2))+
  geom_histogram(aes(y = after_stat(count/max(count))),
                 binwidth = sd(graphDat$GazeX_Shifted)/20,alpha = .25,
                 color = "gray",position = "identity")+
  geom_density(aes(y = after_stat(count/max(count))),
               bw = sd(graphDat$GazeX_Shifted)/20,linewidth = 1,alpha = 1,
               fill = NA,position = "identity") +
  facet_grid(ConditionName~Target)+
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_line(color = alpha("gray", alpha = 0.3)))

