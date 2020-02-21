\name{ExtKur}
\alias{ExtKur}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ExtKur: kurtosis based projection pursuit
%%  ~~function to do ... ~~
}
\description{Returns a data projection with either maximal or minimal kurtosis. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ExtKur(data, iterations, maxmin)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{data matrix
%%     ~~Describe \code{data} here~~
}
  \item{iterations}{number of required iterations
%%     ~~Describe \code{iterations} here~~
}
  \item{maxmin}{is the choice to either maximise ("MAX") or minimise ("MIN") kurtosis
%%     ~~Describe \code{maxmin} here~~
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
\item{linear }{vector of coefficients}
\item{projection}{vector of projected data}
\item{kurt}{extreme kurtosis attainable by a data projection}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Cinzia Franceschini and Nicola Loperfido
%%  ~~who you are~~
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
ExtKur(iris[,1:4],10,"MAX") #returns a data projection with maximal kurtosis
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
