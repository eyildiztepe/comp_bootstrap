# BCa bootstrap CI
boot.BCa <-function(data, statistic, statistic.vector, fun, conf = .95) { 
  data <- as.matrix(data);  n <- nrow(data)
  N <- 1:n;  alpha <- (1 + c(-conf, conf))/2
  zalpha <- qnorm(alpha)
  z0<-qnorm(sum(statistic.vector<statistic)/ length(statistic.vector))
  statistic.vector.jack <- numeric(n)
  for (i in 1:n) { J <- N[1:(n-1)]
    statistic.vector.jack[i] <- fun(data[-i, ], J)   }
  L <- mean(statistic.vector.jack) - statistic.vector.jack
  a <- sum(L^3)/(6 * sum(L^2)^1.5)
  adj.alpha <- pnorm(z0 + (z0+zalpha) / (1-a* (z0 + zalpha)))
  limits <- quantile(statistic.vector, adj.alpha, type=6)
  return(list("estimated"=statistic, "BCa"=limits))    }
