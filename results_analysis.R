
### Process data and summarize it
res <- summarize_data(df_all)

### Show Dataset summary characteristics
vis_data_summary(res$data_summary)

### Visualize results 

#Subset to RF only
res2 = res
res2$results <- res$results[res$results$classifer=="RF"  ,]


#### Visualize Results 
# y: "ROC", "PR", "Brier"
# level: 1 or 2; 1 gives results aggregreated across datasets by N; 2 yields results by dataset for a given N and CV method
# l2_meth: 
vis_results(res2, y= "brier",  level=2, l2_meth="5fold", l2_N = 100, l2_order = "imbalance")

##### Fit mixed model to results #####
test <- analyze(res2, level=1, y="ROC", outcome="coverage", ints=c("cv_method", "n"))
test$int_plot

