#R codes used in the simulation study are provided for #only 20% trimmed mean.
#Distribution: N(0,1), estimator: 20% trimmed mean
#initial values
alpha<-0.05; B<-1500 #bootstrap replications
k<-5000 #iteration number
n<-20 #sample size
tr=0.2 #trim value
theta<-0 # parameter value

#simulation
con.theta <- numeric(B) ; suf.theta <- numeric(B) ; bal.theta <- numeric(B)
con.theta.se<-numeric(B) ; suf.theta.se<-numeric(B) ; bal.theta.se<-numeric(B)
con.bs.bca<-matrix(nrow=k,ncol=2) 
suf.bs.bca<-matrix(nrow=k,ncol=2)
bal.bs.bca<-matrix(nrow=k,ncol=2)
con.bs.t<-matrix(nrow=k,ncol=2)
suf.bs.t<-matrix(nrow=k,ncol=2)
bal.bs.t<-matrix(nrow=k,ncol=2)
con.bs.perc<-matrix(nrow=k,ncol=2)
suf.bs.perc<-matrix(nrow=k,ncol=2)
bal.bs.perc<-matrix(nrow=k,ncol=2)
clist<-list() ; set.seed(100)
for (i in 1:k) { x<-rnorm(n)
  theta.hat <- mean(x,trim=tr)
#Randomly permute the indices for balanced bootstrap
  bal.ind<-sample(rep(1:n,B),B*n)
  for (b in 1:B) { 
    ind.con <- sample(1:n, size = n, replace = TRUE)
    resample.x <- x[ind.con]
    con.theta[b] <- mean(resample.x,trim=tr)
    con.theta.se[b]<-bootse(resample.x,nboot=500, est=mean,SEED=F, trim=tr) 
#bootse computes bootstrap estimate of the standard #error of the estimator est
    ind.suf <- unique(ind.con)#sufficient bootstrapping
    resample.x <- x[ind.suf]
    suf.theta[b] <- mean(resample.x, trim=tr)
    suf.theta.se[b]<-bootse(resample.x,nboot=500, est=mean,SEED=F, trim=tr) 
    #balanced bootstrapping    
    ind.bal<-bal.ind[((b-1)*n+1):(b*n)] 
    resample.x <- x[ind.bal]
    bal.theta[b] <- mean(resample.x,trim=tr)
    bal.theta.se[b]<-bootse(resample.x,nboot=500, est=mean,SEED=F, trim=tr)   }
con.bs.bca[i,]<-boot.BCa(x, statistic = theta.hat, statistic.vector = con.theta, fun = trmean.f, conf=1- alpha)$BCa[c(1,2)]
suf.bs.bca[i,]<-boot.BCa(x, statistic = theta.hat, statistic.vector = suf.theta, fun = trmean.f, conf=1-alpha)$BCa[c(1,2)]
bal.bs.bca[i,]<-boot.BCa(x, statistic = theta.hat, statistic.vector = bal.theta, fun = trmean.f, conf=1-alpha)$BCa[c(1,2)]
con.bs.t[i,]<-boot.t(statistic = theta.hat, statistic.vector = con.theta, se=con.theta.se, conf=1-alpha)$t[c(1,2)]
suf.bs.t[i,]<-boot.t(statistic = theta.hat, statistic.vector = suf.theta, se=suf.theta.se, conf=1-alpha)$t[c(1,2)]
bal.bs.t[i,]<-boot.t(statistic = theta.hat, statistic.vector = bal.theta, se=bal.theta.se, conf=1-alpha)$t[c(1,2)]
con.bs.perc[i,]<-boot.perc(statistic = theta.hat, statistic.vector = con.theta, conf=1-alpha)$percentile[c(1,2)]
suf.bs.perc[i,]<-boot.perc(statistic = theta.hat, statistic.vector = suf.theta, conf=1-alpha)$percentile[c(1,2)]
bal.bs.perc[i,]<-boot.perc(statistic = theta.hat, statistic.vector = bal.theta, conf=1-alpha)$percentile[c(1,2)]
print(paste(i,".completed",sep=""))    }
ci.list<-list(con.bs.bca=con.bs.bca, suf.bs.bca=suf.bs.bca, bal.bs.bca=bal.bs.bca, con.bs.t=con.bs.t, suf.bs.t=suf.bs.t, bal.bs.t=bal.bs.t, con.bs.perc=con.bs.perc, suf.bs.perc=suf.bs.perc, bal.bs.perc=bal.bs.perc)
result <- lapply(ci.list, coverage, value=theta, kr=k)
ci.widths<-lapply(ci.list, ci.width)
result
ci.widths
