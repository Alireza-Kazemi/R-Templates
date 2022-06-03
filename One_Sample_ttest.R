
# example data
Location=rep(c("Area_A","Area_B"),4) 
temp=rnorm(length(Location),34,5) 
sample_data=data.frame(Location,temp)


sample_data %>%
  group_by(Location) %>%                       
  summarise(res = list(tidy(t.test(temp, mu=0)))) %>%
  unnest(cols = c(res))
  
  
  
  
  
  
  
  
sDat$Test = paste(sDat$Health,sDat$Band,sDat$Channel,sep = "_")
Result <- sDat %>%
        group_by(Test) %>%                       
        summarise(res = list(tidy(t.test(value, mu=0)))) %>%
        unnest(cols = c(res))
Result$p.value = round(Result$p.value,3)
Result$Sig = unique("")
Result$Sig[Result$p.value<0.05] =unique("***") 
Result = as.data.frame(Result)

Result$Sig[Result$p.value<0.1] =unique(".") 
Result$Sig[Result$p.value<0.05] =unique("***") 
Result = as.data.frame(Result)
write.csv(Result,"TtestBands_BB.csv",row.names = F)