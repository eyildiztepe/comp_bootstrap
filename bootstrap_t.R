# (Bootstrap-t CI)
boot.t<-function(statistic, statistic.vector, se, conf = .95) {
  t.stats <- (statistic.vector - statistic) /se
  se0 <- sd(statistic.vector)
  Qt <- quantile(t.stats, c((1-conf)/2,(1+conf)/2),type = 6)
  names(Qt) <- rev(names(Qt))
  CI <- rev(statistic - Qt * se0)
  return(list("estimated"=statistic, "t"=CI))  }
