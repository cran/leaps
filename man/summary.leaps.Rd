\name{summary.leaps}
\title{Summary method for a `leaps' object}
\usage{
summary.leaps(ll, all.best=TRUE, matrix=T, matrix.logical=F, show=T, df=NULL)
}
\alias{summary.leaps}
\arguments{
\item{ll}{"leaps" object such as returned by \code{\link{subsets}}}
\item{all.best}{Report all best subsets or just one of each size}
\item{matrix}{Show a matrix indicating which variables are in the model}
\item{matrix.logical}{Use T/F rather than "*" " " in the matrix}
\item{show}{print results}
\item{df}{Number to use instead of number of observations in calculating model choice statistics}
}
\description{
Displays the best subsets found by \code{\link{subsets()}} and returns a logical matrix indicating which variables are in each subset and a collection of popular model choice statistics.
}
\value{
A list with components
\item{which}{logical matrix indicating which variables are in each subset}
\item{rss}{Residual sum of squares for each subset}
\item{rsq}{R-squared for each subset}
\item{adjr2}{Adjusted R-squared for each subset}
\item{cp}{Cp for each subset}
\item{bic}{Bayesian Information Criterion for each subset}
}
\references{
Alan Miller "Subset Selection in Regression" Chapman \& Hall
}
\section{Warning}{The R-squared, Cp and adjusted R-squared are well-known to be useless for model choice after this sort of search. They may be useful for calculating other quantities of interest.}

\seealso{
\code{\link{subsets}}
}
\examples{
data(swiss)
a<-subsets(as.matrix(swiss[,-1]),swiss[,1])
b<-summary(a)
}
