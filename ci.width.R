
ci.width <- function(cis) {  
  # function that takes a matrix   
  #of conf. int. and computes the average of ci width.
  z<-mean(cis[,2]-cis[,1])
  return(z) }
