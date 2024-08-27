
#### Summarize data to be fitted 
# function of  `dfall` target; the target combining results from all simulations 

summarize_data <- function(dfall){
  #fix columna names 
  dfall = janitor::clean_names(dfall)
  
  #get rid of .csv 
  dfall$dataset <- gsub(".csv","", dfall$dataset)
  
  #Calculate bias
  dfall$roc_bias = dfall$sample_roc_auc - dfall$global_roc_auc
  dfall$pr_bias = dfall$sample_pr_auc - dfall$global_pr_auc
  dfall$brier_bias = dfall$sample_brier - dfall$global_brier
  
  #import dataset characteristics
  data_summary = read.csv("results/datasets_summary.csv")
  
  #Merge data summary info withdfall by dataset
  names(data_summary)[2] <- "Dataset"
  
  data_summary = janitor::clean_names(data_summary)
  myVars <- c("number_of_features", "imbalance", "dataset")
  dfall = merge(dfall, data_summary[,myVars], by="dataset", all.x=T)
  
  #calculate N:P
  dfall$np <- as.numeric(dfall$n)/dfall$number_of_features
  
  #Remove datasets with extremely low imbalance 
  dfall = dfall[(dfall$dataset %in% data_summary[data_summary$imbalance>=.03,]$dataset),]
  data_summary = data_summary[data_summary$imbalance>=.03,]
  
  #filter out datasets that didn't run
  dfall = dfall[is.na(dfall$sample_roc_auc)==F,]
  
  
  list(results = dfall,
       data_summary = data_summary
  )
  
}
