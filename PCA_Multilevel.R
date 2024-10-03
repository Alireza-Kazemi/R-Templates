



# head(sDat)
    # SID      ID sessID testName timePoint      CorrL    IncorrL       diff timeStamp      Age  ageBase ageChange   dPrime       CLR
# 1 hc007 hc007t2     t2    Close       900 -0.9413057 -0.8356825 -0.3713892         1 8.906229 8.906229         0 2.699086 0.4583333
# 2 hc007 hc007t2     t2    Close       917 -0.8885738 -0.7835197 -0.3544976         2 8.906229 8.906229         0 2.699086 0.4583333
# 3 hc007 hc007t2     t2    Close       933 -0.8612186 -0.7288911 -0.3659103         3 8.906229 8.906229         0 2.699086 0.4583333
# 4 hc007 hc007t2     t2    Close       950 -0.8001374 -0.6835778 -0.3352874         4 8.906229 8.906229         0 2.699086 0.4583333
# 5 hc007 hc007t2     t2    Close       967 -0.7621658 -0.6498740 -0.3203007         5 8.906229 8.906229         0 2.699086 0.4583333
# 6 hc007 hc007t2     t2    Close       983 -0.7101011 -0.6534500 -0.2633153         6 8.906229 8.906229         0 2.699086 0.4583333

# Convert data to wide for the temporal component
sDatW <- sDat %>%
  pivot_wider(id_cols = c("SID","ID","sessID","testName","ageBase","ageChange","CLR"),
              names_from  = c(timePoint),
              values_from = c(diff,CorrL,IncorrL)) %>% as.data.frame()

myPCAFunc <- function(m){
  # Replace NA values with column means
  m = apply(m, 2, FUN = function(x) { 
    x[is.na(x)] = mean(x, na.rm = TRUE);return(x) })
  m = m[,colSums(is.na(m))==0]
  # Perform PCA
  pca_result <- prcomp(m, center = TRUE, scale. = TRUE)
  # Return the principal components as a data frame
  return(as.data.frame(pca_result$x))
}
sDatN <- sDatW %>%
  group_by(testName, sessID) %>%
  do({
    # Select columns starting with 'diff'
    diff_cols <- select(., starts_with("diff"))
    # Apply the PCA function
    pca_scores <- myPCAFunc(as.matrix(diff_cols))
    # Rename PCA columns (e.g., PC1, PC2, ...)
    colnames(pca_scores) <- paste0("diff_PC", seq_len(ncol(pca_scores)))
    # Bind the PCA scores back to the original data
    bind_cols(., pca_scores)
  }) %>%
  do({
    diff_cols <- select(., starts_with("CorrL"))
    pca_scores <- myPCAFunc(as.matrix(diff_cols))
    colnames(pca_scores) <- paste0("Corr_PC", seq_len(ncol(pca_scores)))
    bind_cols(., pca_scores)
  }) %>%
  do({
    diff_cols <- select(., starts_with("IncorrL"))
    pca_scores <- myPCAFunc(as.matrix(diff_cols))
    colnames(pca_scores) <- paste0("Incorr_PC", seq_len(ncol(pca_scores)))
    bind_cols(., pca_scores)
  }) %>%
  ungroup()
