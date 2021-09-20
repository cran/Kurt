Cum4 <-
function(data,type,shape){#fourth multivariate cumulant
#data = data matrix
#type = 0 uses original data 
#type = 1 uses original centered data 
#type = 2 uses standardized data
#shape= "square" or "rectangular". The output of "square" is a d^2 x d^2 matrix.
  #The output of "rectangular" is a d x d^3 matrix, where d is the number of variables.
#Values in output:
#K4 = fourth cumulant matrix or excess kurtosis matrix

M<-NULL
K4<-NULL
MM<-NULL



rm("M")
rm("K4")
rm("MM")

if (type<0){
print("ERROR, type must be 0 or 1 or 2")
}
if (type>2){
print("ERROR, type must be 0 or 1 or 2")
}

n<-nrow(data)#number of observations
d<-ncol(data)#number of variables
data<-data.matrix(data)
x.mean<-colMeans(data) #mean vector
S<-cov(data)*(n-1)/n #covariance matrix
I<- diag(c(1), ncol=d^2, nrow=d^2)#identity d^2 x d^2 matrix
I2<- diag(c(1), ncol=d, nrow=d)#identity dxd matrix
I3<- diag(c(1), ncol=d^3, nrow=d^3)#identity d^3 x d^3 matrix

####we need the matrixcalc package to use the functions commutation.matrix() and vec().
#####require("matrixcalc")
#####require(matrixcalc)
#####library(matrixcalc)
C<-commutation.matrix(d,d)

if (shape=="square"){###fourth cumulant matrix

if (type==0) {###fourth centered moment matrix

M4<-Fourth4(data,1,"square")
K4<<-M4-(I+C)%*%kronecker(S,S)-vec(S)%*%t(vec(S))
rm(M,envir = as.environment(1), inherits = FALSE)
#####print("Fourth centered moment matrix")
#####return(K4)

multi_returnCum4 <- function() {
  my_listCum4 <- list("Fourth centered moment matrix" = K4)
  return(my_listCum4)
  
}
b<-multi_returnCum4()
return(b)


}
#####}


if (type==1) {###fourth centered moment matrix

M4<-Fourth4(data,1,"square")
K4<<-M4-(I+C)%*%kronecker(S,S)-vec(S)%*%t(vec(S))
rm (M,envir = as.environment(1), inherits = FALSE)
#####print("Fourth centered moment matrix")
#####return(K4)

multi_returnCum4_2 <- function() {
  my_listCum4_2 <- list("Fourth centered moment matrix" = K4)
  return(my_listCum4_2)
  
}
b<-multi_returnCum4_2()
return(b)


}
#####}

if (type==2) {###fourth standardized  moment matrix

M4<-Fourth4(data,2,"square")
K4<<-M4-(I+C)-vec(I2)%*%t(vec(I2))
rm (M,envir = as.environment(1), inherits = FALSE)
#####print("Fourth centered moment matrix")
#####return(K4)

multi_returnCum4_3 <- function() {
  my_listCum4_3 <- list("Fourth centered moment matrix" = K4)
  return(my_listCum4_3)
  
}
b<-multi_returnCum4_3()
return(b)



}
}

#####}

if (shape=="rectangular"){#excess kurtosis matrix
if (type==0){

M4<-Fourth4(data,1,"rectangular")
K4<<-M4-kronecker(S,t(vec(S)))-kronecker(t(vec(S)), S)%*% (I3+kronecker(I2,C))
rm(MM,envir = as.environment(1), inherits = FALSE)
#####print("Excess kurtosis matrix")
#####return(K4)

multi_returnCum4_4 <- function() {
  my_listCum4_4 <- list("Excess kurtosis matrix" = K4)
  return(my_listCum4_4)
  
}
b<-multi_returnCum4_4()
return(b)


}
#####}

if (type==1){

M4<-Fourth4(data,1,"rectangular")
K4<<-M4-kronecker(S,t(vec(S)))-kronecker(t(vec(S)), S)%*% (I3+kronecker(I2,C))
rm (MM,envir = as.environment(1), inherits = FALSE)
#####print("Excess kurtosis matrix")
#####return(K4)

multi_returnCum4_5 <- function() {
  my_listCum4_5 <- list("Excess kurtosis matrix" = K4)
  return(my_listCum4_5)
  
}
b<-multi_returnCum4_5()
return(b)






}

#####}


if (type==2){

M4<-Fourth4(data,2,"rectangular")
K4<<-M4-kronecker(I2,t(vec(I2)))-kronecker(t(vec(I2)), I2)%*% (I3+kronecker (I2,C))
rm(MM,envir = as.environment(1), inherits = FALSE)
#####print("Excess kurtosis matrix")
return(K4)

}

}
}
