\name{Kurt-package}
\alias{Kurt-package}
\alias{Kurt}
\docType{package}
\title{Kurt: Performs kurtosis-based statistical analyses
%\packageTitle{Kurt: Performs kurtosis-based statistical analyses.}
}
\description{Computes measures of multivariate kurtosis, matrices of fourth-order moments and cumulants, kurtosis-based projection pursuit
%\packageDescription{Computes measures of multivariate kurtosis, matrices of fourth-order moments and cumulants, kurtosis-based projection pursuit.}
}
\details{
%The DESCRIPTION file:
%\packageDESCRIPTION{Kurt}
\packageIndices{Kurt}
ScalarKurt(), ExtKurt(), ExtKurtBiv(), optik(), NoKurt(), Cum4(), Fourth(), Fourth4()
}
\author{
Cinzia Franceschini and Nicola Loperfido

Maintainer: Cinzia Franceschini cinziafranceschini@msn.com
}

%}
\references{
Franceschini, C. and Loperfido, N. (2012). On some Inequalities between Measures of Multivariate Kurtosis, with Application to Financial Returns. In "Mathematical and Statistical Methods for Actuarial Sciences and Finance", Perna, C. and Sibillo, M. (Eds.), Springer, 211-218.

    Franceschini, C. and Loperfido, N. (2018). An Algorithm for Finding Projections with Extreme Kurtosis. In "Studies in Theoretical and Applied Statistics: SIS2016-48th Meeting of the Italian Statistical Society, Salerno 8-10 June 2016", Perna C., Pratesi M. and Ruiz-Gazen A. (Eds.), Springer.
    
    Henze, N. (1994). On Mardia's kurtosis test for multivariate normality. Communications in statistics-Theory and Methods 23:4, 1031-1045.
    
    Kollo, T. (2008). Multivariate skewness and kurtosis measures with an application in ICA. Journal of Multivariate Analysis 99, 2328-2338.
    
    Kollo, T. and Srivastava, M.S. (2005). Estimation and testing of parameters in multivariate Laplace distribution. Comm. Statist. 33, 2363--2687.
    
    Koziol, J.A. (1987). An alternative formulation of Neyman's smooth goodness of fit tests under composite alternatives. Metrika 34, 17-24.
    
    Koziol, J.A. (1989). A note on measures of multivariate kurtosis. Biometrical Journal 31, 619-624.
    
    Loperfido, N. (2011). Spectral Analysis of the Fourth Moment Matrix. Linear Algebra and its Applications 435, 1837-1844.
    
    Loperfido, N. (2014). A Note on the Fourth Cumulant of a Finite Mixture Distribution. Journal of Multivariate Analysis 123, 386-394.
    
    Loperfido, N. (2017). A New Kurtosis Matrix, with Statistical Applications. Linear Algebra and its Applications 512, 1-17.
    
    Loperfido N. (2020). Kurtosis-Based Projection Pursuit for Outlier Detection in Financial Time Series. The European Journal of Finance, 26, 142-164.
    
    Loperfido, N. (2020). Some Remarks on Koziol's Kurtosis. Journal of Multivariate Analysis 175,104565.
    
    Malkovich, J.F. and Afifi, A.A. (1973). On Tests for Multivariate Normality. J. Amer. Statist. Ass. 68, 176-179.
    
    Mardia, K.V. (1970). Measures of multivariate skewness and kurtosis with applications. Biometrika 57, 519-530.
    
 Mardia, K. V. and Kent, J. T. (1991). Rao Score Tests for Goodness of Fit and Independence. Biometrika 78, 355-36.
 
 Miettinen J., Taskinen S., Nordhausen K. and Oja H. (2015). Fourth Moments and Independent Component Analysis. Statistical Science 30, 372-390.
     
Mori T.F., Rohatgi V.K. and Szekely G.J. (1993). On multivariate skewness and kurtosis. Theory Probab. Appl. 38, 547-551.
}

\keyword{ package }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
%\examples{
%##run the functions to the iris data
%data(iris)
%iris<-data.matrix(iris)#returns the matrix obtained by converting the data frame to numeric mode
%ScalarKurt(iris[,1:4],"moment","Mardia","upper")#returns the statistic and the p-value of Mardia's kurtosis test for #normality 
%ExtKurt(iris[,1:4],10,"MAX") #returns a data projection with maximal kurtosis
%ExtKurtBiv(iris[,1:2],"MAX")# returns a projection of bivariate data with maximal kurtosis 
%optik(iris[,1:4])# returns a matrix containing the starting values of the iteration aimed at finding projections with                   #extreme kurtosis
%NoKurt(iris[,1:4],3)#returns data projections whose excess kurtosis is as close to zero as possible
%Cum4(iris[,1:4], 1, "square") # returns a matrix containing the fourth cumulants of the given data
%Fourth(iris[,1:4], 1,"square", "moment")#returns a matrix containing the fourth moments. It recalls the function                                              #Fourth4 
%Fourth4(iris[,1:4], 1,"square") #returns matrix containing the fourth moments
%}