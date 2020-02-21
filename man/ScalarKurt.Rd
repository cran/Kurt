\name{ScalarKurt}
\alias{ScalarKurt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ScalarKurt
%%  ~~function to do ... ~~
}
\description{
Returns the statistic and the p-value of either  Mardia's kurtosis  or Koziol's kurtosis tests for normality . 
}
\usage{
ScalarKurt(data, feature, type, prob)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  data matrix
}
  \item{feature}{
"moment" or "cumulant"
}
  \item{type}{
"Mardia" or "Koziol"
}
  \item{prob}{
"lower" if probability is P[X <= x],
"upper" if probability is P[X > x],
"twoside" if probability is computed on both tails
}
}
\details{
For Koziol kurtosis only the upper tail is meaningful
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{statistic}{}
\item{pvalue}{}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Cinzia Franceschini and Nicola Loperfido
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
data(iris)
iris<-data.matrix(iris)#returns the matrix obtained by converting the data frame to numeric mode
ScalarKurt(iris[,1:4],"moment","Mardia","upper")#values of Mardia's kurtosis test for normality 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
