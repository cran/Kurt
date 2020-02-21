\name{Fourth}
\alias{Fourth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fourth: matrices of fourth moments or fourth cumulants
%%  ~~function to do ... ~~
}
\description{
Returns a matrix containing either the fourth moments or the fourth cumulants. It recalls the functions Fourth4 and Cum4. 
}
\usage{
Fourth(data, type, shape, feature)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data matrix
}
  \item{type}{
  type =0 is the ordinary fourth moment / cumulant;
  type =1 is the centered fourth moment / cumulant;
  type =2 is the standardized fourth moment / cumulant
  
%%     ~~Describe \code{type} here~~
}
  \item{shape}{
  "square" or "rectangular"
%%     ~~Describe \code{shape} here~~
}
  \item{feature}{
  "moment" or "cumulant". If feature is "moment", the function computes the fourth moment of a data matrix. The function recalls the function Fourth4. If feature is "cumulant", the function computes the fourth multivariate cumulant. The function recalls the function Cum4.
%%     ~~Describe \code{feature} here~~
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
\item{K4}{Fourth cumulants of the given data}
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
Fourth(iris[,1:4], 1,"square", "moment")#returns a matrix containing the fourth moments
}