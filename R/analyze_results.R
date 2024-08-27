

#### Analyze Outcomes via Linear Mixed Models and plots results #####
# df is list from summarize_data()
# y is outcome to focus on (ROC, PR, or Brier)
# outcome is level 1 outcome to analyzed in summarized data (sd, var, median, IQR, coverage)
# level: level 1 is summarized data across replications; level 2 is within replicatioons nested within datasets at mean level
# ints: is a vector of components to include in an interaction effect

analyze <- function(df, y="roc", outcome="iqr", level = 1, 
                    ints=NULL){
  if(level==1){
    sum_res = res$results %>% 
      rename_at(vars(contains(y)), ~stringr::str_extract(., "[^_]+")) %>% 
      mutate(bias = sample - global) %>%
      group_by(cv_method, n, dataset) %>% 
      summarise(mean = round(mean(bias, na.rm = TRUE), 3),
                sd = round(sd(bias, na.rm = TRUE), 3),
                var = round(var(bias, na.rm=T), 3),
                median = round(median(bias, na.rm = TRUE), 3),
                min = round(min(bias, na.rm = TRUE), 3),
                q1 = round(quantile(bias, .25, na.rm=T),3),
                q3 = round(quantile(bias, .75, na.rm=T),3),
                q05 = round(quantile(bias, .025, na.rm=T),3),
                q95 = round(quantile(bias, .975, na.rm=T),3),
                max = round(max(bias, na.rm = TRUE), 3),
                IQR = q3 - q1, 
                coverage = q95-q05,
                missing = sum(is.na(bias)),
                imbalance = median(imbalance),
                np = median(np))
    
    #Define model formula based on interaction variables
    if(is.null(ints)==F){
      int_term = paste(ints, collapse="*")
      model_form = as.formula(paste(outcome, "~cv_method + n + imbalance + np+", int_term, "+ (1|dataset)", sep=""))
    } else{
      model_form = as.formula(paste(outcome, "~cv_method + n + imbalance + np + (1|dataset)", sep=""))
    }
    
    #Relevel CV method
    sum_res$cv_method <- relevel(as.factor(sum_res$cv_method), ref="split")
    
    #fit model
    fit = lmer(data=sum_res, model_form)
    
    #Construct plots of results
    main_plots = c("cv_method[all]", "n[all]", "imbalance[all]", "np[all]")[c("cv_method", "n", "imbalance", "np") %in% ints == F]
    int_plots = c("cv_method[all]", "n[all]", "imbalance[all]", "np[all]")[c("cv_method", "n", "imbalance", "np") %in% ints == T]
    plot_list <- list()
    for(i in 1:length(main_plots)){
      plot_list[[i]] <- plot_model(fit, type="pred", terms=main_plots[i], title = "")
    }
    
    if(is.null(ints)==F){
      plot_list[[3]] = plot_model(fit, type="pred",terms=int_plots, title="")
    }
    
    #save model results table
    results = tab_model(fit)
    
    out_lab = paste("Outcome: ", toupper(outcome), " of ", y,  sep="")
    
    #output Plots to single table
    if(is.null(ints)==F){
      out_plot = ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]])
      out_plot = annotate_figure(out_plot, top = text_grob(out_lab, 
                                                           color = "black", face = "bold", size = 14))
    } else{
      out_plot = ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]])
      out_plot = annotate_figure(out_plot, top = text_grob(out_lab, 
                                                           color = "black", face = "bold", size = 14))
    }
    
    #Output plot table and results to list
    list(table = results, 
         plots = out_plot,
         int_plot = plot_list[[3]])
    
  } else if(level==2){
    df$results <- df$results %>% 
      rename_at(vars(contains(y)), ~stringr::str_extract(., "[^_]+")) %>% 
      mutate(bias = sample - global)
    
    if(is.null(ints)==F){
      int_term = paste(ints, collapse="*")
      model_form = as.formula(paste("bias~cv_method + n + imbalance + np+", int_term, "+ (1|dataset/Iteration)", sep=""))
    } else{
      model_form = as.formula(paste("bias~cv_method + n + imbalance + np + (1|dataset/iteration)", sep=""))
    }
    
    #Relevel CV method
    df$results$cv_method <- relevel(as.factor(df$results$cv_method), ref="split")
    
    #Fit model
    fit <- lmer(data=df$results, model_form)
    
    #Construct plots of results
    main_plots = c("cv_method[all]", "n[all]", "imbalance[all]", "np[all]")[c("cv_method", "n", "imbalance", "np") %in% ints == F]
    int_plots = c("cv_method[all]", "n[all]", "imbalance[all]", "np[all]")[c("cv_method", "n", "imbalance", "np") %in% ints == T]
    plot_list <- list()
    for(i in 1:length(main_plots)){
      plot_list[[i]] <- plot_model(fit, type="pred", terms=main_plots[i])
    }
    
    if(is.null(ints)==F){
      plot_list[[3]] = plot_model(fit, type="pred",terms=int_plots)
    }
    
    #save model results table
    results = tab_model(fit)
    
    #output Plots to single table
    if(is.null(ints)==F){
      out_plot = ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]])
    } else{
      out_plot = ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]])
    }
    
    #Output plot table and results to list
    list(table = results, 
         plots = out_plot,
         int_plot = plot_list[[3]])
  }
  
}



