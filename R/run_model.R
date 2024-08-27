### Specify CV Method
# Takes defined CV method supplied and defines cv method for cross-validation through tidymodels
spec_cv = function(cv_meth, simdata){
  # Set up cross-validation
  if(cv_meth=="split"){
    cv = mc_cv(simdata, prop = 0.7, times = 1, strata = "y")
  } else if(gsub('[[:digit:]]+','',cv_meth)=="xmccv"){
    k = as.numeric(stringr::str_extract(cv_meth, "[0-9]+"))
    cv = mc_cv(simdata, prop = 0.7, times = k , strata = "y")
  } else if (gsub('[[:digit:]]+','',cv_meth)=="fold"){
    k = as.numeric(stringr::str_extract(cv_meth, "[0-9]+"))
    cv = vfold_cv(simdata, v = k, repeats = 1, strata = "y")
  } else if(gsub('[[:digit:]]+','',cv_meth)=="xfold"){
    nk = as.numeric(str_extract(cv_meth, "(\\d)+(?=x)"))
    k = as.numeric(str_extract(cv_meth, "(\\d)+(?=fold)"))
    cv = vfold_cv(simdata, v = k, repeats = nk, strata = "y")
  } else if(cv_meth=="oob"){
    cv=NULL
  }
  return(cv)
}



### Specify Engine
# Takes supplied engine and specifieds for tidymodels
spec_engine = function(engine){
  if(engine=="RF"){
    # Define model
    rf = rand_forest() %>%
      set_mode("classification") %>%
      set_engine("ranger")
  }else if(engine=="NN"){
    # Define model
    rf = mlp() %>%
      set_mode("classification") %>%
      set_engine("brulee")
  } else if(engine=="Logistic"){
    # Define model
    rf = logistic_reg() %>%
      set_mode("classification") %>%
      set_engine("glm")
  } else if(engine=="Lasso"){
    # Define model
    rf = logistic_reg(penalty = double(1), mixture=double(1)) %>%
      set_mode("classification") %>%
      set_engine("glmnet")
  } 
  return(rf)
}


### SPecify imbalance 
spec_imbalance = function(imb_meth, datarecipe){
  if(imb_meth=="none"){
    imb = datarecipe
  } else if(imb_meth=="down"){
    imb = datarecipe %>% step_downsample(y)
  } else if(imb_meth == "up"){
    imb = datarecipe %>% step_upsample(y)
  }
  return(imb)
}

### Run single iteration of model; inner function within simulation function
run_model <- function(cv_meth, imb_meth="none", n=n, file=file, simdata=simdata, 
                      testdata=testdata, datasplit=datasplit,  engine){
  
  future::plan(sequential)
  
  #Specify CV method
  cv = spec_cv(cv_meth, simdata)

  #Specify Engine
  rf = spec_engine(engine)

  #specify recipe and steps for fitting model
  datarecipe = recipe(y ~., data = simdata) %>% 
    step_other(all_nominal_predictors(), threshold = .05, other="other_2") %>% #covert rare predictor classes w/ <5% response to "other" category 
    step_impute_mode(all_nominal_predictors(), all_factor_predictors()) %>% #mode impute missing nominal predictors
    step_impute_mean(all_numeric_predictors(), all_integer_predictors()) %>% #mean impute missing continuous predictors
    step_zv(all_predictors()) %>% #drop predictors with zero variance 
    step_normalize(all_numeric_predictors(), all_integer_predictors()) %>% #normalize continuous predictors 
    step_dummy(all_nominal_predictors()) #dummy code nominal predictors 

  #add imbalance to recipe 
  datarecipe = spec_imbalance(imb_meth, datarecipe)
  
  #Create workflow 
  rfworkflow = workflow() %>% 
    add_recipe(datarecipe) %>% 
    add_model(rf)
  
  # Fit the model
  if(cv_meth=="oob"){
    rfclass = fit(rfworkflow, simdata)
    preds <- rfclass$fit$fit$fit$predictions[,2]
    prf = ROCR::prediction(preds, simdata$y)
    
    #calculate validation metrics
    validation_roc_auc = performance(prf, "auc")@y.values[[1]]
    validation_pr_auc = performance(prf, "aucpr")@y.values[[1]]
    validation_brier = DescTools::BrierScore( ifelse(simdata$y == levels(simdata$y)[2], 1, 0), preds,scaled=F)
    
    #calculate global metrics
    preds <- unlist(predict(rfclass, testdata, type="prob")[,2])
    prf = ROCR::prediction(preds, testdata$y)
    true_roc_auc =  performance(prf, "auc")@y.values[[1]]
    true_pr_auc =  performance(prf, "aucpr")@y.values[[1]]
    true_brier =  DescTools::BrierScore( ifelse(testdata$y == levels(testdata$y)[2], 1, 0), preds,scaled=F)
    
    converge.warn = 0
    rank.warn = 0
    lossnan.warn = 0
    
  } else {
    rfclass = rfworkflow %>% 
      fit_resamples(resamples = cv, 
                    metrics = metric_set(roc_auc, pr_auc, brier_class), 
                    control = control_resamples(save_pred = TRUE))
    
    error_count = unlist(rfclass$.notes)
    
    converge.warn = sum(grepl("did not converge",error_count))
    rank.warn = sum(grepl("rank-deficient",error_count))
    lossnan.warn = sum(grepl("Current loss",error_count))
    
   
    # Evaluate on validation set
    validation_roc_auc = rfclass %>%
      collect_metrics() %>%
      filter(.metric == "roc_auc") %>%
      pull(mean)
    
    validation_pr_auc = rfclass %>%
      collect_metrics() %>%
      filter(.metric == "pr_auc") %>%
      pull(mean)
    
    validation_brier = rfclass %>%
      collect_metrics() %>%
      filter(.metric == "brier_class") %>%
      pull(mean)
    
    # Predict on test set
    trueclass = last_fit(rfworkflow,
                         split = datasplit,
                         metrics = metric_set(roc_auc, pr_auc, brier_class))
    
    # Evaluate prediction performance
    true_roc_auc = trueclass %>%
      collect_metrics() %>%
      filter(.metric == "roc_auc") %>%
      pull(.estimate)
    
    true_pr_auc = trueclass %>%
      collect_metrics() %>%
      filter(.metric == "pr_auc") %>%
      pull(.estimate)
    
    true_brier = trueclass %>%
      collect_metrics() %>%
      filter(.metric == "brier_class") %>%
      pull(.estimate)
  }
  
  # Populate results data frame with results
  auc_results = list(sample_roc_auc = validation_roc_auc, 
                     global_roc_auc =  true_roc_auc, 
                     sample_pr_auc = validation_pr_auc, 
                     global_pr_auc =  true_pr_auc, 
                     sample_brier = validation_brier, 
                     global_brier =  true_brier, 
                     cv_meth = cv_meth, 
                     n=n,
                     dataset=file,
                     imb_meth = imb_meth,
                     classifer = engine,
                     converge_warn = converge.warn,
                     rank_def_warn = rank.warn,
                     loss_in_nan_warn = lossnan.warn)
  
}
