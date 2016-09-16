lbtest=function(x,type="Ljung-Box",lag=c(6,12)){
  options(digits=4)
  LBvalues<-NULL
  chisqvalues<-NULL
  pvalues<-NULL
  for(i in lag){
    bt<-Box.test(x,type,lag=i)
    LBvalues<-c(LBvalues,bt$statistic)
    pvalues<-c(pvalues,bt$p.value)
    chisqvalues<-c(chisqvalues,qchisq(p=0.95,df=i))
  }
  return(data.frame(lag=lag,"LB.values"=LBvalues,
                    "chisq.values"=chisqvalues,
                    "p.values"=pvalues))
}
