\name{ExtKurBiv}
\alias{ExtKurBiv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ExtKurBiv: kurtosis-based projection pursuit for bivariate random vectors
%%  ~~function to do ... ~~
}
\description{
Returns a projection of bivariate data with either maximal or minimal kurtosis. 
}
\usage{
ExtKurBiv(data, maxmin)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data matrix
}
  \item{maxmin}{choice between maximal ("MAX") and minimal ("MIN") kurtosis

}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{linearMAX}{coefficients of the projections maximising kurtosis}
\item{projectionMAX}{projection with maximal kurtosis}
\item{kurtMAX}{maximal kurtosis}
\item{linearMIN}{coefficients of the projections minimising kurtosis}
\item{projectionMIN}{projection with minimal kurtosis}
\item{kurtMIN}{minimal kurtosis}
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

ExtKurBiv(iris[,1:2],"MAX")# returns a projection of bivariate data with maximal kurtosis 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
