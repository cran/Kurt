\name{Fourth4}
\alias{Fourth4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fourth4: fourth moment of a data matrix
%%  ~~function to do ... ~~
}
\description{
Returns a matrix containing the fourth moments. 
}
\usage{
Fourth4(data, type, shape)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data matrix
}
  \item{type}{
  type=0 is the ordinary fourth moment,
  type=1 is the centered fourth moment,
  type=2 is the standardized fourh moment
%%     ~~Describe \code{type} here~~
}
  \item{shape}{
  "square" or "rectangular"
%%     ~~Describe \code{shape} here~~
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{

%%  ~Describe the value returned
%%  If it is a LIST, use
\item{M}{Fourth square moment matrix}
\item{MM}{Fourth rectangular moment matrix}
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
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
Fourth4(iris[,1:4], 1,"square") #returns a matrix containing the fourth moments
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line