NoKurt <-
function(data,number){
##Data projections whose excess kurtosis is  as close to zero as possible.
##Excess kurtosis is the fourth standardized cumulant, that is the fourth
##standardized moment minus three.
##INPUT:
##data = data matrix (each row represents a unit and each column a variable)
#number = number of required projections. It must be greater than 1 and  less or equal the number of variables-1
##OUTPUT:
##Nkurtoses are...
##Nprojections = matrix of projected data
##MATRIX is the matrix characterizing the projection
##REMARKS:
##1- The projections are arranged in increasing order of their absolute excess kurtosis.
##2- The projections are standardized.
##3- The function calls the function Cum4




  NProjections<-NULL
  Nkurtoses<-NULL
  MATRIX<-NULL
  K4<-NULL
  
  
  rm("NProjections")
  rm("Nkurtoses")
  rm("MATRIX")
  rm("K4")
  
  
data<-data.matrix(data)
n<-nrow(data) #number of units
d<-ncol(data) #number of variables

if(number <=1){ 
message("ERROR, number must be greater than one and smaller than the number of variables")
}
else

if(number>(d-1)) {
message("ERROR, number must be greater than one and smaller than the number of variables")
}
else

x.mean<-apply(data,2,mean)
Z<-sweep(data,2,x.mean)%*%solve(sqrtm(cov(data)*(n-1)/n)) #standardized data
A<-Cum4(data,2,"rectangular")###fourth standardized cumulant
eigen_A<-eigen(A%*%t(A))#eigenvectors and eigenvalues of A
eigenvector_A<-eigen_A$vectors
eigenvalues_A<-eigen_A$values

V<-eigenvector_A[,((ncol(eigenvector_A)-number)+1):ncol(eigenvector_A)]

Projections<-Z%*%V


kurtoses<-apply(Projections,2,kurt)

abskurtoses<-matrix(abs(kurtoses-3),)

order(abskurtoses)
NProjections<<-cbind(Projections[,order(abskurtoses)])#data projections ordered according to the absolute values
#of their excess kurtoses

Nkurtoses<<-apply(NProjections,2,kurt)

ZZ<-data.matrix(cbind(data,NProjections))##
cov_ZZ<-cov(ZZ)*(n-1)/n
cov_XX<-cov_ZZ[1:d,1:d]
cov_XY<-cov_ZZ[1:d,(d+1):ncol(cov_ZZ)]

MATRIX<<-solve(cov_XX)%*%cov_XY

multi_returnNoKurt_1 <- function() {
  my_listNoKurt_1 <- list("Projections" = NProjections, "Kurtoses" = Nkurtoses,"MATRIX"= MATRIX)
  return(my_listNoKurt_1)
  
}
b<-multi_returnNoKurt_1()
return(b)




rm(K4,envir = as.environment(1), inherits = FALSE)


}
