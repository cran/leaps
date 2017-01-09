\name{leaps.setup}
\title{Internal functions for leaps(), subsets()}
\usage{
leaps.setup(x,y,wt=rep(1,length(y)),force.in=NULL,force.out=NULL,intercept=TRUE,nvmax=8,
  nbest=1,warn.dep=TRUE)
leaps.seqrep(leaps.obj)
leaps.exhaustive(leaps.obj,really.big=FALSE)
leaps.backward(leaps.obj,nested)
leaps.forward(leaps.obj,nested)
}
\alias{leaps.setup}
\alias{leaps.exhaustive}
\alias{leaps.forward}
\alias{leaps.backward}
\alias{leaps.seqrep}
\arguments{
\item{x}{A matrix of predictors}
\item{y}{A response vector}
\item{wt}{Optional weight vector}
\item{intercept}{Add an intercept to the model}
\item{force.in}{vector indicating variable that must be in the model}
\item{force.out}{vector indicating variable that must not be in the model}
\item{nbest}{Number of subsets of each size to report}
\item{nvmax}{largest subset size to examine}
\item{warn.dep}{warn if \code{x} is not of full rank}
\item{leaps.obj}{An object of class \code{leaps} as produced by \code{leaps.setup}}
\item{really.big}{required before R gets sent off on a long uninterruptible computation}
\item{nested}{Use just the forward or backward selection models, not the models with variables \code{1:nvmax} constructed for free in the setup}
}
\description{
These functions are used internally by \code{regsubsets} and \code{leaps}. They are wrappers for Fortran routines that construct and manipulate a QR decomposition.}

\seealso{
\code{\link{regsubsets}}, \code{\link{leaps}}
}
\keyword{regression}
