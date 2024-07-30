library(targets)
library(tarchetypes)
library(ranger)
library(tidyverse)
library(tidyr)
library(brulee)
library(tidymodels)
library(dplyr)
library(themis)
library(data.table) 
library(future)
library(future.callr)
library(OpenML)
library(mlr3oml)
library(curl)
library(dplyr)
library(furrr)
library(tidyverse)
library(glue)
library(data.table)
library(stringr)

##### Function for running model#########
run_sims <- function(file, 
                     engine,
                     n=300, 
                     iter=1, 
                     imb_meth="none", 
                     cv_meth="split", 
                     mccv_iter = 10, 
                     nk_iter=10,
                     seed=100){
  
  
  data = fread(paste("datasets/",file, sep=""))
  data = as.data.frame(data)
  
  #convert all characters to factor
  data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], 
                                             as.factor)

  #Set ratio to obtain 300 observations for small data simulation
  prop_n = n/nrow(data)
  
  set.seed(seed) 
  
  #Split data
  datasplit = initial_split(data, prop = prop_n, strata = "y")
  simdata = training(datasplit)
  testdata = testing(datasplit)
  
  out= c(tryCatch({run_model(cv_meth = cv_meth, imb_meth = imb_meth,n=n,mccv_iter=mccv_iter, nk_iter=nk_iter,file=file, simdata=simdata, 
                                  testdata=testdata, datasplit=datasplit, engine=engine)},
                       warning = function(w) { return(list(NA, NA, cv_meth, n, file, imb_meth, engine, NA, NA, NA))}),iter)
  setDT(out)
  
  colnames(out) <- c("Sample AUC", "Global AUC", "CV Method", "N", "Dataset", "Imbalance Method", "Classifer",
                     "Convergence Warning", "Rank Deficient","Loss in NaN Warning", "Iteration")
  
  return(out)
}


### Run Model switch function 
run_model <- function(cv_meth, imb_meth,n=n, mccv_iter=mccv_iter, nk_iter=nk_iter,file=file, simdata=simdata, 
                      testdata=testdata, datasplit=datasplit,  engine){
  
  # Set up cross-validation
  if(cv_meth=="split"){
    cv = mc_cv(simdata, prop = 0.7, times = 1, strata = "y")
  } else if(cv_meth=="mccv"){
    cv = mc_cv(simdata, prop = 0.7, times = mccv_iter , strata = "y")
  } else if (cv_meth=="kfold"){
    cv = vfold_cv(simdata, v = 10, repeats = 1, strata = "y")
  } else if(cv_meth=="nkfold"){
    cv = vfold_cv(simdata, v = 10, repeats = nk_iter, strata = "y")
  }
  
  if(engine=="RF"){
    # Define model
    rf = rand_forest() %>%
      set_mode("classification") %>%
      set_engine("ranger")
    
    datarecipe = recipe(y ~., data = simdata) %>% 
      step_impute_mode(all_nominal_predictors(), all_factor_predictors()) %>% 
      step_impute_mean(all_numeric_predictors(), all_integer_predictors()) %>% 
      step_normalize(all_numeric_predictors(), all_integer_predictors()) 
    
  }else if(engine=="NN"){
    # Define model
    rf = mlp() %>%
      set_mode("classification") %>%
      set_engine("brulee")
    
    datarecipe = recipe(y ~., data = simdata) %>% 
      step_impute_mode(all_nominal_predictors(), all_factor_predictors()) %>% 
      step_impute_mean(all_numeric_predictors(), all_integer_predictors()) %>% 
      step_normalize(all_numeric_predictors(), all_integer_predictors()) %>% 
      step_dummy(all_factor_predictors())
  } else if(engine=="Logistic"){
    # Define model
    rf = logistic_reg() %>%
      set_mode("classification") %>%
      set_engine("glm")
    
    datarecipe = recipe(y ~., data = simdata) %>% 
      step_impute_mode(all_nominal_predictors(), all_factor_predictors()) %>% 
      step_impute_mean(all_numeric_predictors(), all_integer_predictors()) %>% 
      step_normalize(all_numeric_predictors(), all_integer_predictors()) %>% 
      step_dummy(all_factor_predictors())
  }
  
  #control sampling 
  if(imb_meth=="none"){
    daterecipe=datarecipe 
  } else if(imb_meth=="down"){
    datarecipe = datarecipe %>% step_downsample(y)
  } else if(imb_meth == "up"){
    datarecipe = datarecipe %>% step_upsample(y)
  }
  
  #Create workflow
  rfworkflow = workflow() %>% 
    add_recipe(datarecipe) %>% 
    add_model(rf)
  
  # Fit the model
  rfclass = rfworkflow %>% 
    fit_resamples(resamples = cv, 
                  metrics = metric_set(roc_auc), 
                  control = control_resamples(save_pred = TRUE))
  
  error_count = unlist(rfclass$.notes)
  
  converge.warn = sum(grepl("did not converge",error_count))
  rank.warn = sum(grepl("rank-deficient",error_count))
  lossnan.warn = sum(grepl("Current loss",error_count))
  
  # Evaluate on validation set
  validation_auc = rfclass %>%
    collect_metrics() %>%
    filter(.metric == "roc_auc") %>%
    pull(mean)
  
  # Predict on test set
  trueclass = last_fit(rfworkflow,
                       split = datasplit,
                       metrics = metric_set(roc_auc))
  
  # Evaluate prediction performance
  true_auc = trueclass %>%
    collect_metrics() %>%
    filter(.metric == "roc_auc") %>%
    pull(.estimate)
  
  # Populate results data frame with results
  auc_results = list(validation_auc, 
                  true_auc, 
                  cv_meth, 
                  n,
                  file,
                  imb_meth,
                  engine,
                  converge_warning = converge.warn,
                  rank_warning = rank.warn,
                  NaNLoss_warning=lossnan.warn)

}


##### Download datasets based on pre-specified Criteria
datasets_make <- function(){
  #### Pull dataset information 
  tasks = as.data.table(listOMLTasks())
  tasks$percent_missing = tasks$number.of.instances.with.missing.values/tasks$number.of.instances
  tasks$imbalance = tasks$minority.class.size/tasks$number.of.instances
  tasks_subset = subset(tasks,
                        task.type=="Supervised Classification" & 
                          format=="ARFF" &
                          status=="active" &
                          number.of.classes==2 &
                          number.of.instances>10000 &
                          number.of.features<500 & 
                          percent_missing <0.50 & 
                          max.nominal.att.distinct.values < 30
                        
  )

  ### Pull data list that meets criteria
  datalist = as.data.table(listOMLDataSets())
  datalist <- subset(datalist, 
                     data.id %in% tasks_subset$data.id)
  
  #exclude duplicates with most recent version
  setkey(datalist, name, version)
  datalist <- datalist[datalist[, .I[.N], by = .(name)]$V1]
  
  #exclude certain datasets
  rid_did <- datalist[str_detect(datalist$name, "nominal|FOREX|SEA|Hyperplane|house_8L")]$data.id
  rows_drop <- which(datalist$data.id %in% rid_did)
  datalist <- datalist[-rows_drop,]
  
  #Merge in outcome variable from tasks for each data set to datalist
  
  tasks_subset <- tasks_subset %>% select(c(data.id, target.feature, percent_missing, imbalance)) %>% distinct()
  datalist <- left_join(datalist, tasks_subset, by="data.id")
  
  #save data elements file
  fwrite(datalist, "results/datasets_summary.csv")
  
  #establishes list of existing files
  exist_files <- gsub(".csv", "", list.files("datasets/"))
  df_names <- datalist$name
  
  #loop over files and write to folder, skipping if they already exist to save time
  for(i in 1:nrow(datalist)){

    if(df_names[i] %in% exist_files){next}
    temp_df <- getOMLDataSet(data.id=datalist$data.id[1])
    names(temp_df$data)[which(names(temp_df$data)==temp_df$target.features)] <- "y"
    fwrite(temp_df$data, paste("datasets/", df_names[i], ".csv", sep=""), row.names = F)
  }
}





