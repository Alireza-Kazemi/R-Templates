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
