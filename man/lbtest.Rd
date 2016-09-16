\name{lbtest}
\alias{lbtest}
\title{
Ljung-Box for residual test.
}
\description{
Compute the Box每Pierce or Ljung每Box test statistic for examining the null hypothesis of independence in a given time series.
}
\usage{
lbtest(x, type = "Ljung-Box", lag = c(6, 12))
}
\arguments{
  \item{x}{
a numeric vector or univariate time series.
}
  \item{type}{
one of the Box每Pierce or Ljung每Box,Default is Ljung每Box test.
}
  \item{lag}{
the statistic will be based on lag autocorrelation coefficients.Lag can be set at any time ,default is 6 and 12.
}
}

\value{
\item{lag}{the lag.}
\item{LB.values}{the value of the LB test statistic.}
\item{chisq.values}{the value of chisq-values.}
\item{p.values}{the p-value of the test.}
}

\author{
Jinquan Lin
}
\note{
Missing values are not handled.
}


\seealso{
\code{\link{Box.test}}
}

\examples{
##----

data(tableA17)
x=tableA17[2]
lbtest(x)
lbtest(x,lag=1:12)
lbtest(x,lag=c(6,12,18))
lbtest(x,type="Box-Pierce",lag=c(6,12))
}
