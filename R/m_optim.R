m_optim=function(data,x.col,y.col,from=-2,to=2,by=0.5)
{
  if(is.data.frame(data)) data=data else 
  message("Error:data must be a data.frame")
  n<-names(data)[x.col]
  x<-data[n]
  k<-seq(from,to,by)
  likeli<-vector(length=length(k))
  ff<-as.formula(paste(names(data)[y.col],"~.",sep="")) 
  for(i in 1:length(k))
  {
    lm.reg<-lm(ff,data,weights=1/x^k[i])
    likeli[i]<-as.numeric(logLik(lm.reg))
    m<-k[which.max(likeli)]
  }
  return(m)
}
