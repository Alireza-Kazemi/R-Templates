dat = melt(d, id.vars = c("SID","Health","Medication","Stim","Channel","Trial","Vigor","OddEven"),
                variable.name = "Feature")

dat$Feature = gsub("Feature_","",dat$Feature, ignore.case = T)

temp = as.data.frame(summarise(group_by(dat,SID,Health,Medication,Stim,Channel,Trial,Vigor,Feature),N=n()))

dat = reshape2::dcast(dat,SID+Health+Medication+Stim+Channel+Trial+Vigor+OddEven ~ Feature, value.var="value")