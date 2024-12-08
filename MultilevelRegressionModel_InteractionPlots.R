library(pacman)
p_load(data.table,
       reshape2,
       ez,
       lme4,
       car,
       nlme,
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
       ggformula,
       export,
       scales,
       correlation,
       sjPlot,
       interactions,
       jtools)


#----------------------------
#M =  is the output of the lmer function
tab_model(M,show.se = T,show.stat = T,show.est = T)

plot(allEffects(M))

allEffects(M)
plot(effect('ageBase*Similarity',mod = M ,x.var = "Similarity"),
           rug=F,multiline=T,grid=T, 
           ci.style = "bars",
           main="")

plot(effect('ageChange:Similarity',mod = M,x.var = "Similarity",
     xlevels = list(ageChange = c(.5,1,2))),
     rug=F,multiline=T,grid=T, 
     ci.style = "bars",
     main="")



#---------- test Interaction effects -----
A = sim_slopes(M, pred = Similarity, modx = ageBase, 
               # modx.values = c(2.83),
               confint = T, jnplot =T)
print(A)
p = A$jn[[1]]$plot

sapply(p$layers, function(x) class(x$geom)[1])
p$layers[[9]]$aes_params$colour = "#595959"
p = p+annotate(geom="text", x=A$slopes$`Value of ageBase`, y=round(A$slopes$Est.,2), 
               label=paste("ageBase = ",round(A$slopes$`Value of ageBase`+min(sDat$ageBase),2),
                           ", Beta = ",round(A$slopes$Est.,2),sep = ""),
               color="#595959")
plot(p)
WD = "D:\\Plots\\"
graph2ppt(file= paste(WD,"SamplePlot.pptx",sep = ""), width = 7, height = 5)
graph2ppt(file= "SamplePlot.pptx", width = 7, height = 5)


#--------- Plot interaction effects ----

fSize = 20
gDefault =   theme(
  text = element_text(family = "serif"),
  strip.text = element_text(size=fSize-2, face="bold"),
  plot.title = element_text(size = fSize),
  axis.title = element_text(size = fSize),
  legend.title = element_text(size=fSize),
  legend.text=element_text(size=fSize-4),
  axis.text = element_text(size = fSize-4),
  panel.background = element_rect(fill = "transparent", color = "black", 
                                  linewidth = 0.7, linetype =  "solid"),
  plot.background = element_rect(fill = "transparent"),
  axis.line = element_line(linewidth = .7, linetype =  "solid"),
  strip.background = element_rect(fill="transparent"),
  panel.grid.major.x = element_line(color = alpha("gray", alpha = 0.3)),
  panel.grid.minor = element_blank()
)

p =  interact_plot(M, pred = Similarity, modx = ageBase, 
                   # modx.values = c(8,10,12),
                   plot.points = F, point.alpha = 0.2,point.size = .75,point.shape = F,
                   line.thickness = 1.2,vary.lty=F, interval = T,int.type = "confidence",
                   linearity.check =F, rug =F,partial.residuals = F,centered = "all",
                   y.label = "Item-Location Memory Accuracy",
                   x.label = "Differential EMS",
                   colors =c("#7ED7AFFF","#3573A1FF","#3A2C59FF") )
p = p+theme_bw()+gDefault
plot(p)
graph2ppt(file= paste(WD,"EMS_Fig6A.pptx",sep = ""), width = 8, height = 5)
mako(20, alpha = 1, begin = 0, end = 1, direction = 1)
