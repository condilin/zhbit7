\name{print.mysummary.lm}
\alias{print.mysummary.lm}
\title{
Summarize Linear Model
}
\description{
print.summary.lm tries to be smart about formatting the result.
}
\usage{
print.mysummary.lm(x, digits = max(3L, getOption("digits") - 3L), signif.stars = getOption("show.signif.stars"), ...)
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

