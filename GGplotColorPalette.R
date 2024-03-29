
library("RColorBrewer")
display.brewer.all()
  
display.brewer.pal(n = 8, name = 'RdBu')

brewer.pal(n = 8, name = "RdBu")



ggplot(df, aes(x=cond, y=yval, fill=cond)) + geom_bar(stat="identity") +
  scale_fill_brewer(palette="Spectral")

ggplot(df, aes(x=cond, y=yval, fill=cond)) + geom_bar(stat="identity") + 
  scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"))
  
  
  
  
