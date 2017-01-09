\name{regsubsets}
\title{functions for model selection}
\description{Model selection by exhaustive search, forward or backward
  stepwise, or sequential replacement}
\usage{
regsubsets(x=, ...)

\method{regsubsets}{formula}(x=, data=, weights=NULL, nbest=1, nvmax=8,
 force.in=NULL, force.out=NULL, intercept=TRUE,
 method=c("exhaustive", "backward", "forward", "seqrep"),
 really.big=FALSE,
 nested=(nbest==1),...)

\method{regsubsets}{default}(x=, y=, weights=rep(1, length(y)), nbest=1, nvmax=8,
force.in=NULL, force.out=NULL, intercept=TRUE,
 method=c("exhaustive","backward", "forward", "seqrep"),
really.big=FALSE,nested=(nbest==1),...)

\method{regsubsets}{biglm}(x,nbest=1,nvmax=8,force.in=NULL,
method=c("exhaustive","backward", "forward", "seqrep"),
really.big=FALSE,nested=(nbest==1),...)

\method{summary}{regsubsets}(object,all.best=TRUE,matrix=TRUE,matrix.logical=FALSE,df=NULL,...)

\method{coef}{regsubsets}(object,id,vcov=FALSE,...)
\method{vcov}{regsubsets}(object,id,...)

}
\alias{regsubsets}
\alias{regsubsets.default}
\alias{print.regsubsets}
\alias{print.summary.regsubsets}
\alias{regsubsets.formula}
\alias{summary.regsubsets}
\alias{coef.regsubsets}
\alias{vcov.regsubsets}
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
\item{really.big}{Must be TRUE to perform exhaustive search on more than
  50 variables.}
\item{nested}{See the Note below: if \code{nested=FALSE}, models with
  columns 1, 1 and 2, 1-3, and so on, will also be considered} 

\item{object}{regsubsets object}
\item{all.best}{Show all the best subsets or just one of each size}
\item{matrix}{Show a matrix of the variables in each model or just summary
    statistics}
\item{matrix.logical}{With \code{matrix=TRUE}, the matrix is logical
    \code{TRUE}/\code{FALSE} or string \code{"*"}/\code{" "}}
\item{df}{Specify a number of degrees of freedom for the summary
  statistics. The default is \code{n-1}}
\item{id}{Which model or models (ordered as in the summary output) to
  return coefficients and variance matrix for}
\item{vcov}{If \code{TRUE}, return the variance-covariance matrix as an attribute}
\item{...}{Other arguments for future methods}
}
\details{

Since this function returns separate best models of all sizes up to
\code{nvmax} and since different model selection criteria such as AIC,
BIC, CIC, DIC, ... differ only in how models of different sizes are compared, the
results do not depend on the choice of cost-complexity tradeoff.

When \code{x} is a \code{biglm} object it is assumed to be the full
model, so \code{force.out} is not relevant. If there is an intercept it
is forced in by default; specify a \code{force.in} as a logical vector
with \code{FALSE} as the first element to allow the intercept to be
dropped.

The model search does not actually fit each model, so the returned
object does not contain coefficients or standard errors.  Coefficients
and the variance-covariance matrix for one or model models can be
obtained with the \code{coef} and \code{vcov} methods.
}
\note{
As part of the setup process, the code initially fits models with the
first variable in \code{x}, the first two, the first three, and so on.
For forward and backward selection it is possible that the model with the \code{k}
first variables will be better than the model with \code{k}
variables from the selection algorithm. If it is, the model with the
first \code{k} variables will be returned, with a warning. This can
happen for forward and backward selection. It (obviously) can't for
exhaustive search.

With \code{nbest=1} you can avoid these extra models with
  \code{nested=TRUE}, which is the default.
  }
\value{
\code{regsubsets} returns an object of class "regsubsets" containing no
user-serviceable parts. It is designed to be processed by
\code{\link{summary.regsubsets}}.

\code{summary.regsubsets} returns an object with elements
\item{which}{A logical matrix indicating which elements are in each
  model}
\item{rsq}{The r-squared for each model}
\item{rss}{Residual sum of squares for each model}
\item{adjr2}{Adjusted r-squared}
\item{cp}{Mallows' Cp}
\item{bic}{Schwartz's information criterion, BIC}
\item{outmat}{A version of the \code{which} component that is formatted
  for printing}
\item{obj}{A copy of the \code{regsubsets} object}

The \code{coef} method returns a coefficient vector or list of vectors,
the \code{vcov} method returns a matrix or list of matrices.
}

\seealso{
\code{\link{leaps}}
}
\examples{
data(swiss)
a<-regsubsets(as.matrix(swiss[,-1]),swiss[,1])
summary(a)
b<-regsubsets(Fertility~.,data=swiss,nbest=2)
summary(b)

coef(a, 1:3)
vcov(a, 3)
}
\keyword{regression}
