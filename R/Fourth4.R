Fourth4 <-
function(data,type,shape){
#this function needs the expm package
#Fourth moment of a data matrix, that is  a  d^2 x d^2 matrix which includes all moments of order 4,
#where d is the number of variables
#type=0 is the ordinary fourth moment 
#type=1 is the centered fourth moment 
#type=2 is the standardized fourth moment
#shape= "square" or "rectangular"

#Values in output
#M= fourth moment matrix
#MM= fourth moment matrix (rectangular)
  
  
  MM<-NULL
  rm("MM")
  
  data<-data.matrix(data)
size<-dim(data)#dimension of data
n<-nrow(data)#number of observations
d<-ncol(data)#number of variables
A<-matrix(c(0),ncol=d*d,nrow=d*d)#initialitation of the matrix containing the sums of fourth powers
AA<-matrix(c(0),ncol=d^3,nrow=d) #initialitation of the matrix containing the sums of fourth powers, rectangular
y<-matrix(c(0),ncol=d,nrow=n)
if (type==0) {
Y<-data
}
else
if (type==1){
x.mean<-colMeans(data) #mean vector
Y<-sweep(data,2,x.mean)#centered data
}
else ###standardized data
if(type==2){
x.mean<-colMeans(data) #mean vector
Y<-sweep(data,2,x.mean)%*%solve(sqrtm(cov(data)*(n-1)/n)) #standardized data
}

for(i in 1:n){
y<-Y[i,]
a<-kronecker(kronecker(t(y),y),kronecker(t(y),y))#kronecker product
A<-A+a #updates the matrix containing the sums of fourth powers
}

M<-A/n


if (shape=="square"){
M<<-M




#"Fourth moment matrix"
return(M)
}


if(shape=="rectangular"){

for(i in 1:n){
y<-Y[i,]
kron1<-kronecker(t(y),y)
kron2<-kronecker(kron1,t(y))
kron3<-kronecker(kron2,t(y))
AA<-AA+kron3
}

MM<<-AA/n
#"Fourth moment matrix"
return(MM)

}
}
