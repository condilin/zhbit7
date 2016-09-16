#library(lmtest)

diffp=function(x,y,method,model)
{
  if (class(x)=="matrix") x else x=as.matrix(x)
  if (class(y)=="matrix") y else y=as.matrix(y)
  a=c("itera","diff")
  if(method==a[1])
  {
    test=dwtest(model)
    p=1-0.5*test$statistic
  }
  else if(method==a[2]) p=1 
  else message("Error:method must be one of itera or diff")
  x1=p*x[1:nrow(x)-1,]
  y1=p*y[1:nrow(y)-1,]
  x2=x[2:nrow(x),]
  y2=y[2:nrow(y),]
  if(method==a[1])
  {
    Xdt=x2-x1
    Ydt=y2-y1
    return(data.frame(Xdt=Xdt,Ydt=Ydt))
  }else
  {
    Xcf=x2-x1
    Ycf=y2-y1
    return(data.frame(Xcf=Xcf,Ycf=Ycf))
  }
}

