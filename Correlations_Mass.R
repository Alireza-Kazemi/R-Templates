p_load(data.table,
       reshape2,
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
       correlation)
#--------------------------- Correlation Function 
ComputeCorrelations <- function(sDat, Par1Names, Par2Names, sessID, par1ColNames = "Cluster", par2ColName = "Subfield"){
  CorVals = NULL
  for (Par1 in Par1Names)
  {
    for (Par2 in Par2Names)
    {
      Vals = correlation(data = sDat,
                         select = Par1,
                         select2 = c(Par2,"ICV"),
                         partial = T,
                         p_adjust = "none")
      CorVals = rbind(CorVals,Vals[Vals$Parameter2!="ICV",])
    }
  }
  A = as.data.frame(CorVals[CorVals$Parameter2!="ICV",c("Parameter1","Parameter2","r","p","t","df_error","n_Obs","CI_low","CI_high")])
  A$Sig = unique("")
  A$Sig[A$p<0.05] = unique("*")
  A$Sig[A$p<0.01] = unique("**")
  names(A) = c("Pars1", par2ColName, "r", "p","t","df","n_Obs","CI_low","CI_high", "Sig")
  for (idx in 1:length(par1ColNames)){
    A[,par1ColNames[idx]] = sapply(A[,"Pars1"], function(x) strsplit(x,split = "_")[[1]][idx])
  }
  A$sessID = sessID
  A = A[,c(par1ColNames, par2ColName,"sessID","r", "p","t","df","n_Obs","CI_low","CI_high", "Sig")]
  return(A)
}

sDat =  pivot_wider(sDat,id_cols = c("SID","sessID"),
                    names_from = c("testName","MeasureName","clusterID"), 
                    values_from = c("CorrL","IncorrL","Diff"),names_sep = "_")

par1ColNames = c("condition","testName","MeasureName","clusterID")
Par1Names = names(sDat)[!names(sDat) %in% c("SID","sessID")]
Par2Names = names(datVol)[!names(datVol) %in% c("SID", "ICV","sessID")]
sDat = merge(datVol,sDat,by = c("SID","sessID"))

sessID = "t2"
A = ComputeCorrelations(sDat[sDat$sessID==sessID,], Par1Names, Par2Names, sessID,
                        par1ColNames, par2ColName = "Subfield")
	   
#######################################

CorVals = NULL
for (Par1 in c("CorrLoc_Close","CorrLoc_Far","IncorrLoc_Close","IncorrLoc_Far"))
{
  for (Par2 in names(datVol)[!names(datVol) %in% c("SID", "Subject", "subID", "sesID", "CorrLoc_Close","CorrLoc_Far","IncorrLoc_Close","IncorrLoc_Far","ICV")])
  {
    Vals = correlation(data = sDat,
                       select = Par1,
                       select2 = c(Par2,"ICV"),
                       partial = T,
                       p_adjust = "none")
    CorVals = rbind(CorVals,Vals)
  }
}

A = as.data.frame(CorVals[CorVals$Parameter2!="ICV",c("Parameter1","Parameter2","r","p")])
A$Sig = unique("")
A$Sig[A$p<0.05] = unique("*")
write.csv(A,"CorrelationResults.csv",row.names = F)


#######################################	   
	   
corVals = correlation(data = sDat,
                      select = "value", 
                      select2 = c("right_Whole_hippocampus", "left_Whole_hippocampus", 
                                  "Left_Hippocampus_vol", "Right_Hippocampus_vol"))

x = summary(corVals)
layers <- visualisation_recipe(x,
                               show_data = "points",
                               scale = list(range = c(10, 20)),
                               scale_fill = list(
                                 high = "#FF5722",
                                 low = "#673AB7",
                                 name = "r"
                               ),
                               text = list(color = "white"),
                               labs = list(title = "My Plot"))
plot(layers)


# \donttest{
# ==============================================
# Correlation Test
# ==============================================
if (require("see")) {
  rez <- cor_test(mtcars, "mpg", "wt")

  layers <- visualisation_recipe(rez, labs = list(x = "Miles per Gallon (mpg)"))
  layers
  plot(layers)

  plot(rez,
    show_text = "label",
    point = list(color = "#f44336"),
    text = list(fontface = "bold"),
    show_statistic = FALSE, show_ci = FALSE, stars = TRUE
  )
}
#> Loading required package: see

# }
# ==============================================
# Correlation Matrix
# ==============================================
if (require("see")) {
  rez <- correlation(mtcars)

  x <- cor_sort(as.matrix(rez))
  layers <- visualisation_recipe(x)
  layers
  plot(layers)

  #' Get more details using `summary()`
  x <- summary(rez, redundant = TRUE, digits = 3)
  plot(visualisation_recipe(x))

  # Customize
  x <- summary(rez)
  layers <- visualisation_recipe(x,
    show_data = "points",
    scale = list(range = c(10, 20)),
    scale_fill = list(
      high = "#FF5722",
      low = "#673AB7",
      name = "r"
    ),
    text = list(color = "white"),
    labs = list(title = "My Plot")
  )
  plot(layers) + theme_modern()
}
