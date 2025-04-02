#source: https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/
#------------------------ ROC graph ----
bhDat = trialLevelPerformanceDat[,c("pID", "AOI","Condition", "Response", "ResponseBinary", "Performance")]
bhDat$trialLabels = as.numeric(ifelse(bhDat$Condition=="Old", 1, 0))
bhDat$trialID = factor(bhDat$AOI,levels = unique(bhDat$AOI),
                       labels = 1:length(unique(bhDat$AOI))) %>% as.numeric()
bhDat$Response = (bhDat$Response-min(bhDat$Response))/(max(bhDat$Response)-min(bhDat$Response)) 




patientColors = palette.colors(n=6,palette = "Okabe-Ito")
patientColors = patientColors[2:6]
for (pIdx in unique(bhDat$pID)){
  pred <- prediction(predictions = bhDat$Response[bhDat$pID == pIdx], 
                     labels = bhDat$trialLabels[bhDat$pID == pIdx])
  perf <- performance(pred,"tpr","fpr")
  if(pIdx == 1){
    plot(perf,colorize=FALSE,add = FALSE,col = patientColors[pIdx],lwd=2)
  }
  else{
    plot(perf,colorize=FALSE,add = TRUE,col = patientColors[pIdx],lwd=2)
  }
}
legend("bottomright", legend=paste("P_",unique(bhDat$pID),sep = ""),
       col=patientColors, lwd=2)


# patientColors = palette.colors(n=7,palette = "Okabe-Ito")
# patientColors = patientColors[c(2:4,6,7)]
patientColors = viridis(5, alpha = 1, begin = 0, end = .9, direction = -1, option = "D")
for (pIdx in unique(bhDat$pID)){
  roc_obj <- roc(response = bhDat$trialLabels[bhDat$pID == pIdx], 
                 predictor = bhDat$Response[bhDat$pID == pIdx], direction = "<",
                 smooth=TRUE, smooth.n=10, percent = T)
  
  if(pIdx == 1){
    aucInfo = NULL
    A = plot(roc_obj, legacy.axes = TRUE, xlab = "False Alarm Rate (%)", ylab = "Hit Rate (%)", col = patientColors[pIdx],
             asp =NULL,lwd=3,
             xaxs="i",yaxs = "i",
    )
  }
  else{
    A = plot(roc_obj, legacy.axes = TRUE, xlab = "False Alarm Rate (%)", ylab = "Hit Rate (%)", col = patientColors[pIdx], add = TRUE,
             asp =NULL,lwd=3,
             xaxs="i",yaxs = "i",
    )
  }
  aucInfo = c(aucInfo,A$auc)
}
legend("bottomright", legend=paste("Patient ",unique(bhDat$pID),", AUC = ",round(aucInfo),sep = ""),
       col=patientColors, lwd=3)

