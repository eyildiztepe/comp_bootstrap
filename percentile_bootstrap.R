# (Bootstrap percentile CI)
boot.perc <-function(statistic,statistic.vector, conf=.95){
  CI <- quantile(statistic.vector,c((1-conf)/2, (1+conf)/2), type = 6)
  names(CI)<-names(quantile(statistic.vector, c((1-conf)/2, (1+conf)/2)))
  return(list("estimated"=statistic, "percentile"=CI))  }



