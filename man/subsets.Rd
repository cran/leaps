\name{subsets}
\title{functions for subset selection}
\usage{
subsets(x=, ...)

subsets.formula(formula=, data=, weights=rep(1, length(y)), nbest=1, nvmax=8, force.in=NULL, force.out=NULL, intercept=T, method=c("exhaustive", "backward", "forward", "seqrep"), really.big=F)

subsets.default(x=, y=, weights=rep(1, length(y)), nbest=1, nvmax=8, force.in=NULL, force.out=NULL, intercept=T, method=c("exhaustive", "backward", "forward", "seqrep"), really.big=F)
}
\alias{subsets}
\alias{subsets.default}
\alias{subsets.formula}
\arguments{
\item{formula}{model formula for full model}
\item{data}{Optional data frame}
\item{x}{design matrix with all predictors}
\item{y}{response vector}
\item{weights}{weight vector}
\item{nbest}{number of subsets of each size to record}
\item{nvmax}{maximum size of subsets to examine}
\item{force.in}{index to columns of design matrix that should be in all models}
\item{force.out}{index to columns of design matrix that should be in no models}
\item{intercept}{Add an intercept?}
\item{method}{Use exhaustive search, forward selection, backward selection or sequential replacement to search.}
\item{really.big}{Must be T to performe exhaustive search on more than 50 variables.}

}
\description{
Generic function for regression subset selection with methods for formula and matrix arguments. 
}
\value{
An object of class "leaps" containing no user-serviceable parts. It is designed to be processed by \code{\link{summary.leaps}}. If you want to understand the components of this object then read the source.
}
\note{
This function improves on \code{\link{leaps()}} in several ways.  The design matrix need not be of full rank. The ability to restrict \code{nvmax} speeds up exhaustive searches considerably. There is no hard-coded limit to the number of variables.
}
\seealso{
\code{\link{leaps()}}, \code{\link{summary.leaps}}
}
\examples{
data(swiss)
a<-subsets(as.matrix(swiss[,-1]),swiss[,1])
b<-summary(a)
}
