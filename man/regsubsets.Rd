\name{regsubsets}
\title{functions for model selection}
\description{Model selection by exhaustive search, forward or backward
  stepwise, or sequential replacement}
\usage{
regsubsets(x=, ...)

\method{regsubsets}{formula}(x=, data=, weights=NULL, nbest=1, nvmax=8, force.in=NULL, force.out=NULL, intercept=TRUE, method=c("exhaustive", "backward", "forward", "seqrep"), really.big=FALSE,...)

\method{regsubsets}{default}(x=, y=, weights=rep(1, length(y)), nbest=1, nvmax=8,
force.in=NULL, force.out=NULL, intercept=TRUE, method=c("exhaustive",
"backward", "forward", "seqrep"), really.big=FALSE,...)

\method{regsubsets}{biglm}(x,nbest=1,nvmax=8,force.in=NULL,
method=c("exhaustive","backward", "forward", "seqrep"), really.big=FALSE,...)

\method{summary}{regsubsets}(object,all.best=TRUE,matrix=TRUE,matrix.logical=FALSE,df=NULL,...)

}
\alias{regsubsets}
\alias{regsubsets.default}
\alias{print.regsubsets}
\alias{print.summary.regsubsets}
\alias{regsubsets.formula}
\alias{summary.regsubsets}
\alias{regsubsets.biglm}
\arguments{
\item{x}{design matrix or model formula for full model, or \code{biglm} object}
\item{data}{Optional data frame}
\item{y}{response vector}
\item{weights}{weight vector}
\item{nbest}{number of subsets of each size to record}
\item{nvmax}{maximum size of subsets to examine}
\item{force.in}{index to columns of design matrix that should be in all models}
\item{force.out}{index to columns of design matrix that should be in no models}
\item{intercept}{Add an intercept?}
\item{method}{Use exhaustive search, forward selection, backward selection or sequential replacement to search.}
\item{really.big}{Must be TRUE to perform exhaustive search on more than 50 variables.}

\item{object}{regsubsets object}
\item{all.best}{Show all the best subsets or just one of each size}
\item{matrix}{Show a matrix of the variables in each model or just summary
    statistics}
\item{matrix.logical}{With \code{matrix=TRUE}, the matrix is logical
    \code{TRUE}/\code{FALSE} or string \code{"*"}/\code{" "}}
\item{df}{Specify a number of degrees of freedom for the summary
  statistics. The default is \code{n-1}}
\item{...}{Other arguments for future methods}
}
\details{

Since this function returns separate best models of all sizes up to
\code{nvmax} and since different model selection criteria such as AIC,
BIC, ... differ only in how models of different sizes are compared, the
results do not depend on the choice of cost-complexity tradeoff.

When \code{x} is a \code{biglm} object it is assumed to be the full
model, so \code{force.out} is not relevant. If there is an intercept it
is forced in by default; specify a \code{force.in} as a logical vector
with \code{FALSE} as the first element to allow the intercept to be dropped.
}
\value{
An object of class "regsubsets" containing no user-serviceable parts. It is designed to be processed by \code{\link{summary.regsubsets}}. 
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
