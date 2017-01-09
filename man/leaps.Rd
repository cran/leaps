\name{leaps}
\title{all-subsets regressiom}
\usage{
leaps(x=, y=, wt=rep(1, NROW(x)), int=TRUE, method=c("Cp", "adjr2", "r2"), nbest=10,
 names=NULL, df=NROW(x), strictly.compatible=TRUE)
}
\alias{leaps}
\arguments{
\item{x}{A matrix of predictors}
\item{y}{A response vector}
\item{wt}{Optional weight vector}
\item{int}{Add an intercept to the model}
\item{method}{Calculate Cp, adjusted R-squared or R-squared}
\item{nbest}{Number of subsets of each size to report}
\item{names}{vector of names for columns of \code{x}}
\item{df}{Total degrees of freedom to use instead of \code{nrow(x)} in calculating Cp and adjusted R-squared}
\item{strictly.compatible}{Implement misfeatures of leaps() in S}
}
\description{
leaps() performs an exhaustive search for the best subsets of the
variables in x for predicting y in linear regression, using an efficient
branch-and-bound algorithm.  It is a compatibility wrapper for
\code{\link{regsubsets}} does the same thing better.

Since the algorithm returns a best model of each size, the results do
not depend on a penalty model for model size: it doesn't make any
difference whether you want to use AIC, BIC, CIC, DIC, ...
}
\value{
A list with components
\item{which}{logical matrix. Each row can be used to select the columns of \code{x} in the respective model}
\item{size}{Number of variables, including intercept if any, in the model}
\item{cp}{or \code{adjr2} or \code{r2} is the value of the chosen model
  selection statistic for each model}
\item{label}{vector of names for the columns of x}
}
\references{
Alan Miller "Subset Selection in Regression" Chapman \& Hall
}
\note{
With \code{strictly.compatible=T} the function will stop with an error if \code{x} is not of full rank or if it has more than 31 columns. It will ignore the column names of \code{x} even if \code{names==NULL} and will replace them with "0" to "9", "A" to "Z".
}

\seealso{
\code{\link{regsubsets}}, \code{\link{regsubsets.formula}},
\code{\link{regsubsets.default}}
}

\examples{
x<-matrix(rnorm(100),ncol=4)
y<-rnorm(25)
leaps(x,y)
}
\keyword{regression}
