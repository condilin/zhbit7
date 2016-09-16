\name{diffp}
\alias{diffp}
\title{Eliminate autocorrelation}
\description{
diffp is used to eliminate the residual sequence of the existence of self related problems,the method used is Durbin-Watson Test.
}
\usage{
diffp(x, y, method, model)
}

\arguments{
  \item{x}{
independent variable, which can be a data frame or a matrix.
}
  \item{y}{
dependent variable, which can be a data frame or a matrix.
}
  \item{method}{
can choose iterative method or difference method .
}
  \item{model}{
is a regresstion model,use lm method.
And additional arguments to be passed to the dwtest function.So before use to load the dwtest package in the lmtest function.}
}

\value{
  \item{Xdt,Ydt }{a data.frame for method itear}
  \item{Xcf,Ycf}{a data.frame for method diff}
}

\author{
Jinquan Lin
}

\seealso{
\code{\link[lmtest]{dwtest}}
}

\examples{
##---

data(table413)
lm413<-lm(y~x1+x2,data=table413) 
diffp(x=table413[-1],y=table413[1],method="itera",model=lm413)
diffp(x=table413[-1],y=table413[1],method="diff",model=lm413)
}

