#load functions and packages
lapply(list.files("./R", full.names = TRUE), source)

tar#### Create datasets if they don't already exist
#### Will put them into folder "datasets/" in the R Project file
#datasets_make()

#List Files
files <- list.files("datasets/")

## Create grid of all simulations outcomes
analyses <- expand_grid(
  files = files,
  engine=c("RF", "NN", "Logistic"),
  imb_meth=c("none"),
  cv_meth = c("split", "mccv","kfold", "nkfold"),
  n=c(100,300,500),
  iter=1:100,
  mccv_iter=10,
  nk_iter=10
)

## Add file label for naming Targets
analyses$file_label <- gsub(".csv","",analyses$files)

#Apply reproducivle random seed for each iteration
set.seed(071524)
analyses$seed = round(runif(nrow(analyses), 1,10000000))

# n=100
# iter=2
# imb_meth="down"
# cv_meth="kfold"
# mccv_iter = 10
# nk_iter=10
# seed=10
# 
# out = run_sims(file=files[5],
#                engine="RF",
#           n=100,
#           iter=2,
#           imb_meth="down",
#           cv_meth="kfold",
#           mccv_iter = 10,
#           nk_iter=10,
#           seed=10)



branch_resources <- tar_resources(
    future = tar_resources_future(resources = list(n_cores=16))
  )


tar_plan(
  bm <- tar_map(
    values=analyses[analyses$engine=="RF",],
    names=c(n, engine, imb_meth, cv_meth, file_label, iter),
    tar_target(
      bm,
      run_sims(files,engine, n, iter,
               imb_meth, cv_meth, 
               mccv_iter, nk_iter, 
               seed),
      resources=branch_resources,
      memory="transient",
      garbage_collection = T
    )
  ),
  tar_combine(bm_comb, bm[[1]])
)

library(usethis)


