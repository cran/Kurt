\name{optik}
\alias{optik}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{optik 
%%  ~~function to do ... ~~
}
\description{
It computes the matrix containing the smallest and largest kurtoses of data projections
as well as the corresponding directions. 
}
\usage{
optik(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data matrix
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{kurMAX}{kurtosis of the projection maximizing kurtosis}
\item{pMAX}{projection maximizing kurtosis}
\item{dMAX}{direction maximizing kurtosis}
\item{kurMINbis}{kurtosis of the projection minimizing kurtosis}
\item{pMINbis}{projection minimizing kurtosis}
\item{dMINbis}{direction minimizing kurtosis}
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
optik(iris[,1:4])#startingvalues of the iteration aimed at finding projections with extreme kurtosis}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line