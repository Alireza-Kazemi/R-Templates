
Sdat      = read.csv("DataFile.csv",sep = ",",header=TRUE,strip.white=TRUE)

results=as.data.frame(ezANOVA(data=Sdat, dv="value", wid=.("ID"), within=.("condition"),
                              between = c("age_group"), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num] =lapply(results[is.num], round, 3)
results



#################### With missing values ##########

Sdat = reshape2::dcast(FeatDat,Band+Stim+Health ~ Run, value.var="value")
Sdat = melt(Sdat, id.vars = c("Health","Stim","Band"),
            variable.name = "Run")
Sdat$value[is.na(Sdat$value)]=0

results=as.data.frame(ezANOVA(data=Sdat, dv="value", wid=.("Run"), within=.("Band"),
                              between = c("Stim","Health"), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num] =lapply(results[is.num], round, 3)
results