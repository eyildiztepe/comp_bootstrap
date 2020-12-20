trmean.f<-function(d,i){
  z<-mean(d[i], trim=0.2)
  return(z)
}
