\name{plot.regsubsets}
\alias{plot.regsubsets}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Graphical table of best subsets}
\usage{
\method{plot}{regsubsets}(x, labels=obj$xnames, main=NULL, scale=c("bic", "Cp", "adjr2", "r2"),
col=gray(seq(0, 0.9, length = 10)),...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
 \item{x}{\code{regsubsets} object }
 \item{labels}{variable names}
 \item{main}{title for plot }
 \item{scale}{which summary statistic to use for ordering plots}
 \item{col}{Colors: the last color should be close to but distinct from
   white }
 \item{...}{other arguments}
}
\description{
Plots a table of models showing which variables are in each model. The
models are ordered by the specified model selection statistic. This plot 
is particularly useful when there are more than ten or so models and the simple table 
produced by \code{\link{summary.regsubsets}} is too big to read.
}
\value{
None
}
\author{Thomas Lumley, based on a concept by Merlise Clyde}
\seealso{\code{\link{regsubsets}},\code{\link{summary.regsubsets}} }

\examples{
data(swiss)
a<-regsubsets(Fertility~.,nbest=3,data=swiss)
par(mfrow=c(1,2))
plot(a)
plot(a,scale="r2")
}
\keyword{hplot}
\keyword{regression}
