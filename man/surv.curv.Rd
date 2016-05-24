\name{surv.curv}
\alias{surv.curv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
survival curves
}
\description{
Cacluate survival proportion of Reaction Time (RT) data in each bin
}
\usage{
surv.curv(x, .bin, sample = FALSE, sample.n = length(x), replace = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A numeric vector of RT data.
}
  \item{.bin}{
A numeric vector specifying upper bounds of each timing bin
}
  \item{sample}{
logical. If \code{TRUE}, random sample of \code{x} is used to calculate survival proportion.
}
  \item{sample.n}{
A size of the sample when \code{sample=TURE}. Default to \code{length(x)}
}
  \item{replace}{
logical. If \code{TRUE}, sampling (in case of \code{sample=TURE}) will be with replacement.
}
}
\details{
This funciton is used internally to calculate the survival proportion.
}
\value{
A vector of length \code{length(.bin)} corresponding to survival proportion at each of timing bin in \code{.bin}.
}

\seealso{
\code{\link{surv.mean}}
}
\examples{
#generate a random RT data
set.seed(1)
sbj.x <- rnorm(n = 50, mean=300, sd=50)

#obtain a survival curve for 600 bins form 0 ms to 600 ms.
curv.x <- surv.curv(sbj.x, .bin=seq(0,600,by=1))

#plot
plot(curv.x)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
