
library(pacman)
p_load(tidyr,
       dplyr)


value = c(1,5,34,5,6,3,5,7,4,76,8,54,3,6,8,6,4,3,6,7,98,56,4,3,6,7,89,65,4,3,5,6,87,85)
r1 = list(tidy(t.test(value)))
r2 = list(tidy(t.test(value)))
A = c(1,2)
B = data.frame(A)
B$res = c(r1,r2)
A = unnest(B,cols = c(res))



# A very good application in multiple t-test across conditions

sDat = dat
sDat$value = FisherZ(sDat$M_Diff)
sDat$Test = paste(sDat$Health,sDat$Medication,sDat$Stim,sDat$Channel,sDat$PAC,sep = "_")
Result <- sDat %>%
  group_by(Test) %>%                       
  summarise(res = list(tidy(t.test(value, mu=0)))) %>%
  unnest(cols = c(res))