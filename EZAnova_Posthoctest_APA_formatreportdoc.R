
library(pacman)
p_load(reshape2,
       ez,
       lme4,
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
       # officer,
       psych,
       rstatix,
       emmeans,
       standardize,
       performance,stringr,
       scatterplot3d,
       plotrix,
       rgl,RColorBrewer,export)
	   

###################################################### Functions ------
PostHocT_Tests <- function(data, CompList = NULL, Paired = T, PAdjustM = "bonferroni", VarEqual = T,
                           Detailed = T, PoolSD = F, Alternative = "two.sided",PrintOutput = T){
  results <- data  %>%
    pairwise_t_test(value ~ groups, pool.sd = PoolSD, var.equal=VarEqual, paired = Paired,
                    detailed = Detailed,
                    alternative = Alternative,
                    comparisons = CompList,
                    p.adjust.method = PAdjustM) %>%
    mutate_if(is.numeric, round,digits = 3) %>%
    as.data.frame()
  
  CD <- data %>%
    cohens_d(value~groups, var.equal = VarEqual,paired = Paired,
             comparisons = CompList) %>%
    as.data.frame()
  
  marginals <- data %>%
    group_by(groups)%>%
    summarise(M = mean(value, na.rm=T),
              SD = sd(value, na.rm=T)) %>%
    mutate_if(is.numeric, round,digits = 2) %>%
    as.data.frame()
  
  results = results[,c("group1", "group2", "statistic", "df", "p", "p.adj", "p.adj.signif", "conf.low", "conf.high")]
  results = results %>% mutate_at(c("statistic", "df", "conf.low", "conf.high"), round,digits = 2)
  results$cohenD = round(CD$effsize,2)
  if(PrintOutput){
    print(results)
    print(marginals)
  }
  out = list(results,marginals)
}
WriteANOVAResults_APA <- function(file = "ANOVAoutput.doc", results , boldFlag = T){
  #Requires library(rtf)
  rtfFile <- RTF(file)
  results = results[results$Effect!="(Intercept)",]
  results$interaction = grepl(":",results$Effect)
  mainEffects = results$Effect[results$interaction==0]
  for (factorName in mainEffects){
    tempRes = results[results$Effect==factorName,]
    addText(rtfFile,"There was ")
    if (tempRes$p<0.05){
      addText(rtfFile,"a ")
      addText(rtfFile,"significant " , bold = boldFlag)
    }
    else{
      addText(rtfFile,"no " , bold = boldFlag)
    }
    addText(rtfFile,paste("main effect of ",factorName,", ",sep = "") )
    addText(rtfFile,"f", italic = T)
    addText(rtfFile,paste("(",tempRes$DFn,", ",tempRes$DFd,") = ", tempRes$F,", ",sep = "") )
    addText(rtfFile,"p ", italic = T)
    if (tempRes$p<0.001){
      addText(rtfFile," < 0.001, ")
    }
    else{
      addText(rtfFile,paste(" = ",tempRes$p,", ",sep = "") )
    }
    addText(rtfFile,paste("#pareta = ",tempRes$pareta,"\n\n",sep = "") )
  }
  
  
  interEffects = results$Effect[results$interaction==1]
  for (factorName in interEffects){
    tempRes = results[results$Effect==factorName,]
    facNames = gsub(":"," and ",factorName)
    addText(rtfFile,"There was ")
    if (tempRes$p<0.05){
      addText(rtfFile,"a ")
      addText(rtfFile,"significant " , bold = boldFlag)
    }
    else{
      addText(rtfFile,"no " , bold = boldFlag)
    }
    addText(rtfFile,paste("interaction effect between ", facNames,", ",sep = ""))
    addText(rtfFile,"f", italic = T)
    addText(rtfFile,paste("(",tempRes$DFn,", ",tempRes$DFd,") = ", tempRes$F,", ",sep = "") )
    addText(rtfFile,"p ", italic = T)
    if (tempRes$p<0.001){
      addText(rtfFile,"< 0.001, ")
    }
    else{
      addText(rtfFile,paste(" = ",tempRes$p,", ",sep = "") )
    }
    addText(rtfFile,paste("#pareta = ",tempRes$pareta,"\n\n",sep = "") )
  }
  done(rtfFile)
}

WriteTTestResults_APA <- function(file = "Ttestoutput.doc", results,marginals, boldFlag = T){
  #Requires library(rtf)
  rtfFile <- RTF(file)
  
  for (rIdx in 1:nrow(results)){
    d = results[rIdx,]
    mG1 = marginals[marginals$groups==d$group1,]
    mG2 = marginals[marginals$groups==d$group2,]
    
    addText(rtfFile,paste(d$group1,", (",sep = "") )
    addText(rtfFile,"M ", italic = T)
    addText(rtfFile,paste("= ",mG1$M,", ",sep = "") )
    addText(rtfFile,"SD ", italic = T)
    addText(rtfFile,paste("= ",mG1$SD,"), were ",sep = "") )
    if(d$p.adj>0.05){
      addText(rtfFile,"not significantly ", bold = boldFlag)
    }else{
      addText(rtfFile,"significantly ", bold = boldFlag)
    }
    addText(rtfFile,paste("different from ",d$group2,", (",sep = "") )
    addText(rtfFile,"M ", italic = T)
    addText(rtfFile,paste("= ",mG2$M,", ",sep = "") )
    addText(rtfFile,"SD ", italic = T)
    addText(rtfFile,paste("= ",mG2$SD,"); ",sep = "") )
    addText(rtfFile,"t", italic = T)
    addText(rtfFile,paste("(",d$df,") = ",d$statistic,", ",sep = "") )
    addText(rtfFile,"p ", italic = T)
    if (d$p<0.001){
      addText(rtfFile,"< 0.001, ")
    }
    else{
      addText(rtfFile,paste(" = ",d$p,", ",sep = "") )
    }
    
    addText(rtfFile,"Cohen's ")
    addText(rtfFile,"d ", italic = T)
    addText(rtfFile,paste(" = ",d$cohenD,", ",sep = "") )
    
    addText(rtfFile,"#padj ", italic = T)
    if (d$p.adj<0.001){
      addText(rtfFile,"< 0.001, ")
    }
    else{
      addText(rtfFile,paste(" = ",d$p.adj,", ",sep = "") )
    }
    addText(rtfFile,"95% CI ", italic = T)
    addText(rtfFile,paste("[",d$conf.low,", ",d$conf.high,"]\n\n",sep = "") )
    
  }
  done(rtfFile)
}

################### Usage ------


sDat = graphdat
head(sDat)
results=as.data.frame(ezANOVA(data=sDat, dv="value", wid=.("itt"), within=.("Stim","PAC"),
                              between = c("Health"), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num] =lapply(results[is.num], round, 3)
results

WriteANOVAResults_APA("0_Anova.doc",results = results)

#------------ posthoc test
phDat = sDat
head(phDat)
phDat$groups = paste(phDat$Health,phDat$Stim,sep = "_")
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = F)
WriteTTestResults_APA(file = "0_ttest.doc", A[[1]],A[[2]])