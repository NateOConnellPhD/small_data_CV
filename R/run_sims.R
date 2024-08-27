


##### Function for running Simulations  #####
### Outer simulation function ###
# Function of...
# file: this is the .csv file of the global dataset to read in
# engine: "RF", "NN", "Lasso"
# n: the size of the subsample to take from the global data
# iter = 1: The number of replications to run per scenario
# imb_meth: "none", "up", "down", "smote", "rose"; determine the type of method to handle class imbalance
# cv_meth: method of split sampling; "split", "kfold", "oob" (For RF engines only)', "nkfold", "mccv"
# mccv_iter: 
# nk_iter: 
# seed: random seed for replication reproducibility 

run_sims <- function(file, 
                     engine="RF",
                     n=300, 
                     iter=1, 
                     imb_meth="none", 
                     cv_meth="10fold", 
                     seed=100){
  
  #Necessary future plan that allows targets outside of the function to run in parallel
  future::plan(sequential)
  
  #read in dataset from file
  data = fread(paste("datasets/",file, sep=""))
  data = as.data.frame(data)
  
  #convert all characters to factor
  data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], 
                                             as.factor)
  
  #Convert outcome to factor
  data$y <- as.factor(data$y)
  
  #Set ratio to obtain 300 observations for small data simulation
  prop_n = n/nrow(data)
  
  #Set random seed for reporudicibility
  set.seed(seed) 
  
  #Define shell outcome table 
  out = data.table(sample_roc_auc = numeric(), global_roc_auc = numeric(), 
                   sample_pr_auc = numeric(), global_pr_auc = numeric(),
                   sample_brier = numeric(), global_brier = numeric(),
                   cv_meth = character(), n = numeric(), dataset = character(), 
                   imb_meth = character(), classifer = character(), converge_warn = numeric() , rank_def_warn = numeric(), 
                   loss_in_nan_warn = numeric())
  
  for(i in 1:iter){
    
    #Split data
    datasplit = initial_split(data, prop = prop_n, strata = "y")
    simdata = training(datasplit)
    testdata = testing(datasplit)
    
    #Record outcome data; if model has issues doesnt run, fill in with NAs
    out_temp = c(tryCatch({run_model(cv_meth = cv_meth, imb_meth = imb_meth,n=n, file=file, simdata=simdata, 
                                     testdata=testdata, datasplit=datasplit, engine=engine)},
                          warning = function(w) { return(list(NA, NA, NA, NA, NA, NA, cv_meth, n, file, imb_meth, engine, NA, NA, NA))}))
    setDT(out_temp)
    names(out_temp) = names(out)
    out = out %>% bind_rows(out_temp)
    
  }
  
  out$iter = seq(1, iter)
  
  colnames(out) <- c("Sample ROC AUC", "Global ROC AUC",
                     "Sample PR AUC", "Global PR AUC",
                     "Sample Brier", "Global Brier",
                     "CV Method", "N", "Dataset", "Imbalance Method", "Classifer",
                     "Convergence Warning", "Rank Deficient","Loss in NaN Warning", "Iteration")
  
  return(out)
}