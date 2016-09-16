#library(leaps)

aic=function(x,y,...){
  if (class(x)=="matrix") x else x=as.matrix(x)
  if (class(y)=="matrix") y else y=as.matrix(y)
  l=summary(regsubsets(x,y,...))
  d=l$which
  d=d[,-1]
  rss_SSE=l$rss
  p=vector(length=nrow(d))
  for(i in 1:nrow(d)){
    if(length(table(d[i,]))==2){
      k=as.numeric(table(d[i,])[2])
    } else
    {
      k=as.numeric(table(d[i,])[1])
    }
    p[i]=k
  }
  AIC=nrow(x)*log(rss_SSE)+2*p
  return(list(AIC=round(AIC,2),p=p))
}
