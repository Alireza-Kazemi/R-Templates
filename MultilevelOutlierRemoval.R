#--------------------------- Remove Outliers 
RemoveOutliers <- function(data,factorNames,varNames,Criteria=3.29){
  # datTemp = data[,c(factorNames,varNames)]
  # Conds = NULL
  # for (condName in factorNames){
  #   Conds = paste(Conds,datTemp[,condName],sep = "_")    
  # }
  # 
  # for (condName in unique(Conds)){
  #   for (varName in varNames){
  #     X = datTemp[Conds==condName,varName]
  #     X[as.vector(abs(scale(X, center=TRUE, scale=TRUE)))>Criteria]=NA
  #     data[Conds==condName,varName] = X
  #   }
  # }
  # Using mutate
  data = data %>% group_by_at(factorNames)%>%
    mutate_at(varNames,
              ~ ifelse(as.vector(abs(scale(.,center = TRUE, scale = TRUE)))>3,NA,.)) %>%
    as.data.frame()
  return(data)
}


  
  
