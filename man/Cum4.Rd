\name{Cum4}
\alias{Cum4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cum4: fourth multivariate cumulant
%%  ~~function to do ... ~~
}
\description{returns a matrix containing the fourth cumulants of the given data
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Cum4(data, type, shape)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{data matrix
%%     ~~Describe \code{data} here~~
}
  \item{type}{type=0 uses original data, type=1 uses centered data, type=2 uses standardized data
%%     ~~Describe \code{type} here~~
}
  \item{shape}{if shape="square" the output is a d^2 x d^2 matrix. If shape="rectangular", the output is a d x d^3 matrix. Where d is the number of variables
%%     ~~Describe \code{shape} here~~
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{K4 is the matrix containing the fourth cumulants of the given data
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
Cum4(iris[,1:4], 1, "square") # returns a matrix containing the fourth cumulants of the given data
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
