rnchi=function(x,k,a)
{
  z=cut(x,0:k/k,labels = FALSE)
  z[is.na(z)]=0
  O=c()
  for (i in 1:k) {
    O[i]=sum(z==i)
  }
  E=c()
  for (i in 1:k) {
    E[i]=(1/k)*length(x)
  }
  chi.squared=sum((O-E)^2/E)
  if(chi.squared>qchisq(a/2,df=k-1) && chi.squared<qchisq(a/2,df=k-1,lower.tail = FALSE)){
    return(list("ACCEPTED",chi.squared))
  }
  else{
    return(list("REJECTED",chi.squared))
  }
}
rnchi(lcg1,10,0.05)
