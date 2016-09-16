\name{aic}
\alias{aic}
\title{Akaike's  Information Criterion}
\description{
The AIC value of the whole model in the selection of independent variables
}
\usage{
aic(x, y, ...)
}

\arguments{
  \item{x}{
independent variable, which can be a data frame or a matrix.
}
  \item{y}{
dependent variable, which can be a data frame or a matrix.
}
  \item{...}{
see regsubsets}
}

\value{
\item{aic}{the result AIC and p returned is numeric}
 \item{p}{numeric}
}

\author{
Jinquan Lin
}

\seealso{
\code{\link{regsubsets}}, \code{\link{leaps}},\code{\link{nobs}}

}
\examples{
##----

data(table51)
x=table51[1:3]
y=table51[4]						
aic(x=x,y=y,nbest=3,method= "exhaustive")
}

													
