\name{regsubsets}
\title{functions for model selection}
\usage{
regsubsets(x=, ...)

regsubsets.formula(x=, data=, weights=rep(1, length(y)), nbest=1, nvmax=8, force.in=NULL, force.out=NULL, intercept=TRUE, method=c("exhaustive", "backward", "forward", "seqrep"), really.big=FALSE,...)

regsubsets.default(x=, y=, weights=rep(1, length(y)), nbest=1, nvmax=8,
force.in=NULL, force.out=NULL, intercept=TRUE, method=c("exhaustive",
"backward", "forward", "seqrep"), really.big=FALSE,...)

summary.regsubsets(object,all.best=TRUE,matrix=TRUE,matrix.logical=FALSE,df=NULL,...)

}
\alias{regsubsets}
\alias{regsubsets.default}
\alias{print.regsubsets}
\alias{print.summary.regsubsets}
\alias{regsubsets.formula}
\alias{summary.regsubsets}
\arguments{
\item{x}{design matrix or model formula for full model}
\item{data}{Optional data frame}
\item{y}{response vector}
\item{weights}{weight vector}
\item{nbest}{number of subsets of each size to record}
\item{nvmax}{maximum size of subsets to examine}
\item{force.in}{index to columns of design matrix that should be in all models}
\item{force.out}{index to columns of design matrix that should be in no models}
\item{intercept}{Add an intercept?}
\item{method}{Use exhaustive search, forward selection, backward selection or sequential replacement to search.}
\item{really.big}{Must be T to perform exhaustive search on more than 50 variables.}

\item{object}{regsubsets object}
\item{all.best}{Show all the best subsets or just one of each size}
\item{matrix}{Show a matrix of the variables in each model or just summary
    statistics}
\item{matrix.logical}{With \code{matrix=TRUE}, the matrix is logical
    \code{TRUE}/\code{FALSE} or string \code{"*"}/code{" "}}
\item{df}{Specify a number of degrees of freedom for the summary
  statistics. The default is \code{n-1}}
\item{...}{Other arguments for future methods}
}
\description{
Generic function for regression subset selection with methods for formula and matrix arguments. 
}
\value{
An object of class "regsubsets" containing no user-serviceable parts. It is designed to be processed by \code{\link{summary.regsubsets}}. 
}
\note{
This function improves on \code{\link{leaps}} in several ways.  The design matrix need not be of full rank. The ability to restrict \code{nvmax} speeds up exhaustive searches considerably. There is no hard-coded limit to the number of variables.
}
\seealso{
\code{\link{leaps}}
}
\examples{
data(swiss)
a<-regsubsets(as.matrix(swiss[,-1]),swiss[,1])
summary(a)
b<-regsubsets(Fertility~.,data=swiss)
summary(a)
}
\keyword{regression}