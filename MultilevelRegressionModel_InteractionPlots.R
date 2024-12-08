library(pacman)
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
# b = ggplot_build(p)
# p$data[[9]]$colour = "#595959"
# p = ggplot_gtable(p)
plot(p)
WD = "D:\\Projects\\"
graph2ppt(file= paste(WD,"FileName.pptx",sep = ""), width = 7, height = 5)


#--------- Plot interaction effects ----


res = probe_interaction(M, pred = EMS, modx = ageBase, modx.values = c(8,10,12),
                        plot.points = F, point.alpha = 0.5,point.size = .5,point.shape = T,
                        line.thickness = 1,vary.lty=F, interval = F,int.type = "confidence",
                        linearity.check =F, rug =F,partial.residuals = T,centered = "all")

p =  interact_plot(M, pred = EMS, modx = ageBase, 
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
