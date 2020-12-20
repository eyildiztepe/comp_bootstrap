coverage <- function(ci, value,kr=nrow(ci)) {
#function that takes a matrix of confidence intervals and #the true value of parameter
#and returns the coverage level.
  coverage<-(kr-(sum((ci[,1]>value)|(ci[,2]<value))))/kr
  below<-(sum(ci[,2]<value))/kr
  over<-(sum(ci[,1]>value))/kr
  return(list("coverage" = coverage, "below" = below,  "over"= over))   }
