#load functions and packages
lapply(list.files("./R", full.names = TRUE), source)
library(future)
library(future.callr)
future::plan(callr)

#### Create datasets if they don't already exist
#### Will put them into folder "datasets/" in the R Project file
# if datasets have already been created/exist with same default name, then they will not be over-written
#datasets_make()

#List Files
files <- list.files("datasets/")

#ignore file sizes larger than 200,000 KB for computation Feasibility
files <- files[file.size(list.files("./datasets/", full.names=T))<200000000]

#Get 20 smallest datasets
#files = files[order(file.size(list.files("./datasets/", full.names=T)))[21:30]]

#Define CV methods to use
cv_methods = make_cvmethods(split=T, oob=F, nkfold=F, mccv=F, 
                            k=c(seq(2,10,1), seq(12, 20, 2), 25, 30, 50, 75, 99))

## Create grid of all simulations outcomes
analyses <- expand_grid(
  files = files,
  engine=c("RF"),
  imb_meth=c("none"),
  cv_meth = cv_methods,
  iter=50,
  n=c(100),
)

## Add file label for naming Targets
analyses$file_label <- gsub(".csv","",analyses$files)
analyses$file_label <- gsub("-", "_", analyses$file_label)
analyses$file_label <- gsub("\\(", "_", analyses$file_label)
analyses$file_label <- gsub("\\)", "", analyses$file_label)

## subset oob to only RF
analyses <- rbind(analyses[analyses$cv_meth=="oob" & analyses$engine=="RF",], analyses[analyses$cv_meth!= "oob",])

#create seed list for random splits (original seed: 071524) * DO NOT CHANGE SEED OR IT WILL NEED TO RECOMPUTE ALL TARGETS *
set.seed(071524)
seeds = sample(seq(1:10000),100,replace=F)


### Branch resources
branch_resources <- tar_resources(
  future = tar_resources_future(resources = list(n_cores=20))
)

#tar plan
tar_plan(
  bm <- tar_map(
    values=analyses,
    names=c(n, engine, imb_meth, cv_meth, file_label),
    tar_target(
      bm,
      run_sims(files, engine, n, iter,
               imb_meth, cv_meth, 
               seeds),
      resources=branch_resources,
      memory="transient",
      garbage_collection = T
    )
  ),
  tar_combine(df_all, bm[[1]])
)



