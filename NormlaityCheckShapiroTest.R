CheckNormality <- function(dataFrame,factorNames,varName){
  datTemp = dataFrame[,c(factorNames,varName)]
  Conds = NULL
  for (condName in factorNames){
    Conds = paste(Conds,datTemp[,condName],sep = "_")    
  }
  datout = NULL
  for (condName in unique(Conds)){
    X = datTemp[Conds==condName,varName]
    A = shapiro.test(X)
    datout = rbind(datout,data.frame("condition" = condName,"pValue" = A$p.value,
                                     "Is_Skewed_P" = ifelse(A$p.value<=0.05,"*","-")))
    dataFrame[Conds==condName,"Is_Skewed_P"] = unique(A$p.value)
  }
  return(datout)
}