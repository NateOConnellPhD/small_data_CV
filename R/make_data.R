
##### Download datasets based on pre-specified Criteria from open ML

datasets_make <- function(){
  
  #### Pull dataset information
  
  tasks = listOMLTasks(task.type="Supervised Classification", number.of.instances = c(10000,1000000), number.of.features = c(5,500))
  tasks$percent_missing = tasks$number.of.instances.with.missing.values/tasks$number.of.instances
  tasks$imbalance = tasks$minority.class.size/tasks$number.of.instances
  tasks_subset = subset(tasks,
                        number.of.classes==2 &
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
  
  rid_did <- datalist[str_detect(datalist$name, "kr-vs|nominal|FOREX|SEA|Hyperplane|house_8L|SPECT|spect")]$data.id
  rows_drop <- which(datalist$data.id %in% rid_did)
  datalist <- datalist[grepl("Sparse", datalist$format)==F, ]
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
    temp_df <- getOMLDataSet(data.id=datalist$data.id[i])
    names(temp_df$data)[which(names(temp_df$data)==temp_df$target.features)] <- "y"
    
    #get rid of perfect predictors (no variance)
    if(temp_df$desc$name== "nursery"){
      temp_df$data$health <- NULL
    }
    
    fwrite(temp_df$data, paste("datasets/", df_names[i], ".csv", sep=""), row.names = F)
    
  }
  
}