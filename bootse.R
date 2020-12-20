bootse<-function(x,nboot=1000,est=median,SEED=TRUE,...){
#  Wilcox, R.R. (2017). Introduction to Robust Est. and #Hypothesis Testing. 4th Ed.  
#   Rallfun-vxx.txt can be downloaded from 
#  http://dornsife.usc.edu/labs/rwilcox/software/ 
#   Compute bootstrap estimate of the standard error of the estimator est
  if(SEED)set.seed(2) # set seed of random number #generator so that results can be duplicated.
  data<-matrix(sample(x,size=length(x)*nboot, replace=TRUE), nrow=nboot)
  bvec<-apply(data,1,est,...)
  bootse<-sqrt(var(bvec))
  bootse}
