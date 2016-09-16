\name{mysummary.lm}
\alias{mysummary.lm}
\title{
Summarize Linear Model Fits
}
\description{
mysummary method for class "lm"
}
\usage{
mysummary.lm(regmodel, ...)
}

\arguments{
  \item{regmodel}{
an object of class "lm", usually, a result of a call to \code{\link{lm}}.
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{
print.mysummary.lm tries to be smart about formatting the result.
See more of  anova function and summary.lm function.
}

\author{Jinquan Lin}

\seealso{
objects to See Also as \code{\link{anova}}, \code{\link{summary.lm}}
}
\examples{
##---- 

data(table36)
lm36 <- lm(y~.,data=table36)
mysummary(lm36)
}
