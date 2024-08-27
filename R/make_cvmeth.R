
### Function that defines cvmethods to read into analyses dataframe to run simulations on
# split, mccv, oob, kfold, and nkfold are logical T or F 
# if mccv = T, then nsplit is a scalar or vector defining the number of split-samples to run in repeates of MCCV
# if kfold=T, then k is a scalar or vector of varying 'k's to run in kfold
# if nkfold=T, the nk is scalar or vector of the varying 'k's to run in nkfold (the `k` in the nxkfold), and `nnk` is the vector contains the number of repeated kfolds to run (the n in the nxkfold)

make_cvmethods = function(split = T, mccv = T, oob = T, kfold=T, nkfold=T,
                          nsplit = 10, k = 10, nk= 10, nnk=10) {
  
  mccv_out = paste(nsplit, "xmccv", sep="")
  kfold_out = paste(k, "fold", sep="")
  nkfold_out = paste(nnk,"x",rep(nk, length(nnk)), "fold", sep="")
    
  return(c("split", mccv_out, "oob", kfold_out, nkfold_out)[c(split, rep(mccv, length(nsplit)), oob, rep(kfold, length(k)), rep(nkfold, length(nk)*length(nnk)))])
  
}

