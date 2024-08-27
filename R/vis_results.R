
#### Summary figures for results #####
# y: "roc", "pr", or "brier"; yields either AUC for ROC or PR, or Brier Score bias
# level: 1 gives summary data at the method and N level; 2 gives summary at dataset level
# type: "custom" or "std". "custom" yields customized boxplots based on summarized data, "std" yields standard boxplots from GGPlot2 with outliers
# l2_meth: define which method to summarize at the dataset level if level =2
# l2_N: define for what N to summarize at the dataset level if level =1
# l2_fill: define what outcome to differentiate by color in the box plots ("median_y", "np", "imbalance")
# l2_order: metric to determine order of y-axis 

vis_results <- function(df, y="roc", 
                        level=1, 
                        type="custom",
                        l2_meth = "10fold", 
                        l2_N = "300", 
                        l2_fill = "median_y",
                        l2_order = "imbalance"){
  
  if(level==1){
    p_title = paste("Comparison of ", ifelse(y %in% c("ROC", "PR"), paste(y,"-AUC", sep=""), "Brier Score"), 
                    " by CV Methods and Sample Size Aggregated \nAcross Replications and Datasets")
  } else if(level==2){
    p_title = paste("Comparison of ", ifelse(y %in% c("ROC", "PR"), paste(y,"-AUC", sep=""), "Brier Score"), 
                    " Across Datasets for ", l2_meth, " CV and N=", l2_N, sep="")
  }

  df$results = df$results %>% 
    rename_at(vars(contains(y)), ~stringr::str_extract(., "[^_]+")) %>% 
    mutate(bias = sample-global) 
  
  if(level==1){
    #Summary at level 1 (by CV method and N)
    sum_data_l1 = df$results %>% 
      rename_at(vars(contains(y)), ~stringr::str_extract(., "[^_]+")) %>% 
      mutate(bias = sample-global) %>%
      group_by(cv_method, n) %>% 
      summarise(mean_bias = round(mean(bias, na.rm = TRUE), 3),
                sd_bias = round(sd(bias, na.rm = TRUE), 3),
                median_bias = round(median(bias, na.rm = TRUE), 3),
                min_bias = round(min(bias, na.rm = TRUE), 3),
                q1_bias = round(quantile(bias, .25, na.rm=T),3),
                q3_bias = round(quantile(bias, .75, na.rm=T),3),
                q05_bias = round(quantile(bias, .025, na.rm=T),3),
                q95_bias = round(quantile(bias, .975, na.rm=T),3),
                max_bias = round(max(bias, na.rm = TRUE), 3),
                missing = sum(is.na(bias)))
    
    #establish CV method Order for Plotting
    all_methods = unique(sum_data_l1$cv_method)
    split_include = "split" %in% all_methods
    oob_include = "oob" %in% all_methods
    mccv_include = "mccv" %in% gsub('[[:digit:]]+','',all_methods)
    kfold_include = "fold" %in% gsub('[[:digit:]]+','',all_methods)
    nkfold_include = "xfold" %in% gsub('[[:digit:]]+','', all_methods)
    
    mccv_all = kfold_all = nkfold_all = oob_all = split_all = NULL
    
    if(oob_include==T) oob_all = "oob"
    
    if(split_include==T) split_all = "split"
    
    if(mccv_include==T){
      mccv_all = all_methods[ gsub('[[:digit:]]+','',all_methods) %in% "mccv"]
      mccv_all = mccv_all[order(as.numeric(stringr::str_extract(mccv_all, "[0-9]+")))]
    }
    
    if(kfold_include==T){
      kfold_all = all_methods[ gsub('[[:digit:]]+','',all_methods) %in% "fold"]
      kfold_all = kfold_all[order(as.numeric(stringr::str_extract(kfold_all, "[0-9]+")))]
    }
    
    if(nkfold_include==T){
      nkfold_all = all_methods[ gsub('[[:digit:]]+','',all_methods) %in% "xfold"]
      nkfold_all = nkfold_all[order(as.numeric(str_extract(nkfold_all, "(\\d)+(?=fold)")))]
      nkfold_all = nkfold_all[order(as.numeric(str_extract(nkfold_all, "(\\d)+(?=x)")))]
    }

    method_order = c(oob_all, split_all, mccv_all, kfold_all, nkfold_all)
    
    sum_data_l1$cv_method = factor(sum_data_l1$cv_method, method_order)
    
    
  }else if(level==2){
    #SUmmary at level 2 (by CV method, N, and Dataset)
    sum_data_l2 = df$results %>% 
      rename_at(vars(contains(y)), ~stringr::str_extract(., "[^_]+")) %>% 
      mutate(bias = sample-global) %>%
      group_by(cv_method, n, dataset) %>% 
      summarise(mean_bias = round(mean(bias, na.rm = TRUE), 3),
                sd_bias = round(sd(bias, na.rm = TRUE), 3),
                median_bias = round(median(bias, na.rm = TRUE), 3),
                min_bias = round(min(bias, na.rm = TRUE), 3),
                q1_bias = round(quantile(bias, .25, na.rm=T),3),
                q3_bias = round(quantile(bias, .75, na.rm=T),3),
                q05_bias = round(quantile(bias, .025, na.rm=T),3),
                q95_bias = round(quantile(bias, .975, na.rm=T),3),
                max_bias = round(max(bias, na.rm = TRUE), 3),
                missing = sum(is.na(bias)), 
                mean_y = mean(global),
                median_y = median(global),
                IQR = q3_bias - q1_bias, 
                missing = sum(is.na(bias)),
                imbalance = median(imbalance),
                np = median(np))
    
    #establish CV method Order for Plotting
    all_methods = unique(sum_data_l2$cv_method)
    split_include = "split" %in% all_methods
    oob_include = "oob" %in% all_methods
    mccv_include = "mccv" %in% gsub('[[:digit:]]+','',all_methods)
    kfold_include = "fold" %in% gsub('[[:digit:]]+','',all_methods)
    nkfold_include = "xfold" %in% gsub('[[:digit:]]+','', all_methods)
    
    mccv_all = kfold_all = nkfold_all = oob_all = split_all = NULL
    
    if(oob_include==T) oob_all = "oob"
    
    if(split_include==T) split_all = "split"
    
    if(mccv_include==T){
      mccv_all = all_methods[ gsub('[[:digit:]]+','',all_methods) %in% "mccv"]
      mccv_all = mccv_all[order(as.numeric(stringr::str_extract(mccv_all, "[0-9]+")))]
    }
    
    if(kfold_include==T){
      kfold_all = all_methods[ gsub('[[:digit:]]+','',all_methods) %in% "fold"]
      kfold_all = kfold_all[order(as.numeric(stringr::str_extract(kfold_all, "[0-9]+")))]
    }
    
    if(nkfold_include==T){
      nkfold_all = all_methods[ gsub('[[:digit:]]+','',all_methods) %in% "xfold"]
      nkfold_all = nkfold_all[order(as.numeric(str_extract(nkfold_all, "(\\d)+(?=fold)")))]
      nkfold_all = nkfold_all[order(as.numeric(str_extract(nkfold_all, "(\\d)+(?=x)")))]
    }
    
    method_order = c(oob_all, split_all, mccv_all, kfold_all, nkfold_all)
    
    sum_data_l2$cv_method = factor(sum_data_l1$cv_method, method_order)
    
  }
  
  ylab = ifelse(y %in% c("roc", "pr"), paste(toupper(y),"-AUC", sep=""), "Brier Score")
  
  
  if(level==1 & type == "custom"){
    
    
    cap = paste("Whiskers represent 95% Coverage \nBox represents IQR (50% Coverage) \nCenter line represents median; Red dot represents mean")
    
    p1 <- ggplot(sum_data_l1, 
                 aes(y =cv_method)) + 
      geom_boxplot(aes(xmin=q05_bias, xlower=q1_bias, xmiddle=median_bias, xupper=q3_bias, xmax=q95_bias), stat="identity") +
      stat_summary(data=df$results,aes(x=bias, y=cv_method), fun.data = "mean_se", 
                   geom = "pointrange", 
                   colour = "red") +
      facet_grid(rows = vars(n)) +
      geom_vline(xintercept = 0, 
                 linetype = "dotted", 
                 color = "blue",
                 size = 0.7) +
      scale_x_continuous(breaks=seq(from=-0.2, 0.2, .05)) + 
      ggtitle(p_title) + 
      labs(x= paste(ylab, " Bias", sep=""), caption = cap) + 
      theme_bw()
  }else if(level==1 & type== "std"){
    cap = paste("Standard Boxplot parameters for `geom_boxplot` \nWhiskeres extend 1.5*IQR \nBox repsents IQR")
    
    #(Combined) Box plot for median & IQR and point range for mean & SE
    p1 <- ggplot(df$results, 
                 aes(x = bias, y = cv_method)) + 
      geom_boxplot(outliers = T, notch=T, outlier.alpha = .2)+
      stat_summary(fun.data = "mean_se", 
                   geom = "pointrange", 
                   colour = "red") +
      expand_limits(x = c(-0.2, 0.2)) +
      labs(title = "Comparison of CV methods",
           x = "Bias", 
           y = "CV Method") +
      facet_grid(rows = vars(n))+
      scale_x_continuous(breaks=seq(from=-0.2, 0.2, .05)) + 
      geom_vline(xintercept = 0, 
                 linetype = "dotted", 
                 color = "blue",
                 size = 0.7) +
      theme_bw() + 
      labs(x= paste(ylab, " Bias", sep=""), caption = cap) + 
      ggtitle(p_title)
    
  }else if(level==2){
    
    cap = paste("Whiskers represent 95% Coverage \nBox represents IQR (50% Coverage) \nCenter line represents median")
    
    y_title = ifelse(l2_order=="median_y", paste("Median True ",toupper(y), "-AUC", sep=""), 
                     ifelse(l2_order=="np", "N:P Ratio", 
                            ifelse(l2_order=="imbalance", "Imbalance", "Other")))
    legend_title = ifelse(l2_fill=="median_y", paste("Median True \n",toupper(y), "-AUC", sep=""), 
                          ifelse(l2_fill=="np", "N:P Ratio", 
                                 ifelse(l2_fill=="imbalance", "Imbalance", "Other")))
    
    sum_data_l2 = sum_data_l2[sum_data_l2$n==as.character(l2_N),]
    sum_data_l2 = sum_data_l2[sum_data_l2$cv_method==l2_meth,]
    sum_data_l2$dataset <- factor(sum_data_l2$dataset, sum_data_l2[order(eval(parse(text=paste("sum_data_l2$",l2_order, sep="")))),]$dataset)
    
    p1 <- ggplot(sum_data_l2[sum_data_l2$cv_method==l2_meth & sum_data_l2$n==as.character(l2_N),], 
                 aes(y = dataset, fill=eval(parse(text=paste(l2_fill))))) + 
      scale_y_discrete(labels=as.character(sort(round(eval(parse(text=paste("sum_data_l2$",l2_order, sep=""))),2)))) + 
      geom_boxplot(aes(xmin=q05_bias, xlower=q1_bias, xmiddle=median_bias, xupper=q3_bias, xmax=q95_bias), stat="identity") +
      geom_vline(xintercept = 0, 
                 linetype = "dotted", 
                 color = "blue",
                 size = 0.7) +
      scale_x_continuous(breaks=seq(from=-0.2, 0.2, .05)) + 
      theme_bw() + 
      labs(x=paste(ylab, " Bias", sep=""), y = y_title , fill=legend_title, caption=cap) + 
      ggtitle(p_title) 
    
  }
  
  return(p1)
}
