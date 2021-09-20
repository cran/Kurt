optik <-
function(data){
#Matrix containing the smallest and largest kurtosis of data projections
#as well as the corresponding directions.
#data= data matrix
#Values in output:
#kurMAX= kurtosis of the projection maximizing kurtosis
#pMAX = projection maximizing kurtosis
#dMAX =direction maximizing kurtosis
#kurMINbis= kurtosis of the projection minimizing kurtosis
#pMINbis = projection minimizing kurtosis
#dMINbis = direction minimizing kurtosis






#library(expm) ###we need the solve command in the expm package
#####library(labstatR)##we need the function kurt() in the labstatR package

pMINbis<-NULL
  pMAX<-NULL
  kurMAX<-NULL
  dMAX<-NULL
  kurMINbis<-NULL
  dMINbis<-NULL
  
  rm("pMINbis")
  rm("pMAX")
  rm("kurMAX")
  rm("dMAX")
  rm("kurMINbis")
  rm("dMINbis")


  data<-data.matrix(data)
n<-nrow(data)#number of observations
d<-ncol(data)#number of variables

x.mean<-colMeans(data) #mean vector
data<-data.matrix(data)#we transform data into a data matrix object
#Z1<-data%*%sqrtm(solve(cov(data)*(n-1)/n))
Z<-sweep(data,2,x.mean)%*%sqrtm(solve(cov(data)*(n-1)/n)) #standardized data
A<-matrix(c(0),ncol=d*d,nrow=d*d)#initialization of the matrix containing fourth powers
M<-matrix(c(0),ncol=d,nrow=d)
Mbis<-matrix(c(0),ncol=d,nrow=d)

for(i in 1:n){
z<-Z[i,] #i-th unit
a<-kronecker(kronecker(t(z),z),kronecker(t(z),z))#kronecker product
A<-A+a #updates the sum matrix of the fourth powers
}
FST<-A/n #fourth standardized moment

eigen(FST) #spectral decomposition of FST
eigen_vectors<-eigen(FST)$vectors


e<-eigen_vectors[,1]#dominant eigenvector of FST

ebis<-eigen(FST)$vectors[,(d*(d+1)/2)]

if(e[1] < 0){

e<-e*-1
}


for(i in 1:d){
M[,i]<-e[(((i-1)*d)+1):(d*i)] #i-th column of the squared vector
Mbis[,i]<-ebis[(((i-1)*d)+1):(d*i)]###????

 }

M<-(M+t(M))/2 #correction for asymmetry
Mbis<-(Mbis+t(Mbis))/2

Mbis<-Mbis%*%Mbis

eigenM<-eigen(M)#spectral decomposition of M
eigenMbis<-eigen(Mbis)#spectral decomposition of Mbis

eigen_vectors_M<-eigen(M)$vectors
eigen_vectors_Mbis<-eigen(Mbis)$vectors

e1<-eigen_vectors_M[,1] #dominant eigenvector of M
e2bis<-eigen_vectors_Mbis[,1]

pMAX<<-Z%*%e1#projection maximizing kurtosis

pMINbis<<-Z%*%e2bis#projection minimizing kurtosis

dMAX<<-((sqrtm(solve(cov(data)*(n-1)/n)))%*%e1)/norm((sqrtm(solve(cov(data)*(n-1)/n))%*%e1),type="F")#direction maximizing kurtosis

dMINbis<<-((sqrtm(solve(cov(data)*(n-1)/n)))%*%e2bis)/norm((sqrtm(solve(cov(data)*(n-1)/n))%*%e2bis),type="F")#direction minimizing kurtosis

kurMINbis<<-kurt(pMINbis)

kurMAX<<-kurt(pMAX)

multi_return <- function() {
  my_list <- list("kurtosis of the projection maximizing kurtosis" = kurMAX, "projection maximizing kurtosis" = pMAX, "direction maximizing kurtosis" = dMAX,"kurtosis of the projection minimizing kurtosis" = kurMINbis, "projection minimizing kurtosis" = pMINbis, "direction minimizing kurtosis" = dMINbis)
  return(my_list)
 
}
 a<-multi_return()
return(a)

}
