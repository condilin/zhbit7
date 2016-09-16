\name{mysummary}
\alias{mysummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Statistics Summaries 
}
\description{
summary is a generic function used to produce result summaries of the results of various model fitting functions.
}
\usage{
mysummary(object, ...)

## S3 method for class 'lm'
mysummary(object, ...)
}

\arguments{
  \item{object}{
an object for which a summary is desired.
}
  \item{\dots}{
additional arguments affecting the mysummary produced.
}
}
\details{
summarize the results produced by lm.
}

\author{Jinquan Lin}

\seealso{
summarize function \code{\link{mysummary.lm}}, \code{\link{anova}}
}
\examples{
##---- 

data(table36)
lm36 <- lm(y~.,data=table36)
mysummary(lm36)
}

