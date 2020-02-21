\name{NoKurt}
\alias{NoKurt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{NoKurt: data projections whose excess kurtosis is as close to zero as possible
%%  ~~function to do ... ~~
}
\description{
Data projections whose excess kurtosis is as close to zero as possible. Excess kurtosis is the fourth standardized cumulant , that is the fourth standardized moment minus three.
}
\usage{
NoKurt(data, number)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data matrix
}
  \item{number}{
number of required projections. It must be greater than one and less than the number of variables
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{Nkurtoses}{kurtoses of Nprojections}
\item{Nprojections}{data projections ordered according to the absolute values of their excess kurtoses}
\item{MATRIX}{matrix characterizing the projection}
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
iris<-data.matrix(iris[,1:4])#returns the matrix obtained by converting the dframe to numeric mode
NoKurt(iris[,1:4],3)#returns data projections whose excess kurtosis is as close to zero as possible
}