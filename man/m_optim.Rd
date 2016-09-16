\name{m_optim}
\alias{m_optim}
\title{Function to derive the m index}
\description{
m_optim is use to find out the index of the independent variable when the variance of the error term is proportional to the independent variable.
}
\usage{
m_optim(data, x.col, y.col, from = -2, to = 2, by = 0.5)
}

\arguments{
  \item{data}{
Must be a data frame, and the data data frame contains only the independent variable and the dependent variable, which can not contain other independent variables.
}
  \item{x.col}{
independent variable of columns in the data .
}
  \item{y.col}{
dependent variable of columns in the data .
}
  \item{from,to}{
the starting and (maximal) end values of the sequence.
}
  \item{by}{
number: increment of the sequence.}
}

\author{Jinquan Lin}

\seealso{
The model fitting function \code{\link{lm}} ;\code{\link{summary.lm}} 
}
\examples{
##----

data(table36)
m_optim(data=table36,x.col=2,y.col=3)
m_optim(data=table36,x.col=2,y.col=3,from=1,to=5,by=0.5)
}


