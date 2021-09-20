ExtKurBiv <-
function(data,maxmin){
#Kurtosis-based projection pursuit for bivariate random vectors.
#In input we have:
#data =  data matrix with two variables
#maxmin = choice between maximal ("MAX") and minimal ("MIN") kurtosis
#Values in output are:
#linearMAX= coefficients of the projection maximising kurtosis
#linearMIN= coefficients of the projection minimising kurtosis
#projectionMAX=  the projection with maximal kurtosis
#projectionMIN= the projection with minimal kurtosis
#kurtMAX= maximal kurtosis attainable by a data projection (M)
#kurtMIN= minimal kurtosis attainable by a data projection  (m)


linearMAX<-NULL
  projectionMAX<-NULL
  linearMIN<-NULL
  projectionMIN<-NULL
  kurtMAX<-NULL
  kurtMIN<-NULL
  rm("linearMAX")
  rm("projectionMAX")
  rm("linearMIN")
  rm("projectionMIN")
  rm("kurtMAX")
  rm("kurtMIN")



if(ncol(data)!=2){
print("ERROR, data must be a matrix with two variables ")
}

  # requireNamespace("polynom","expm","labstatR","MASS")

#library(MASS)#we need this package 
#library(polynom)#package needed to find solutions of the polynomial
#####library(expm)#we need this package
#####library(labstatR)#package needed to use the function kurt
data<-data.matrix(data)
n<-nrow(data)#number of units
x.mean<-apply(data,2,mean)#mean vector
Z<-sweep(data.matrix(data),2,x.mean)%*%solve(sqrtm(cov(data)*(n-1)/n)) #standardized data
z1<-Z[,1]#first standardized variable
z2<-Z[,2]#second  standardized variable

m1111<-mean(z1*z1*z1*z1)
m1112<-mean(z1*z1*z1*z2)
m1122<-mean(z1*z1*z2*z2)
m1222<-mean(z1*z2*z2*z2)
m2222<-mean(z2*z2*z2*z2)
a<-m1112#fourth degree term's coefficient
b<-3*m1122-m1111 #third degree term's coefficient
c<-3*(m1222-m1112)#second degree term's coefficient
d<-m2222-3*m1122#first degree term's coefficient
e<--m1222#null degree term's coefficient

pol<-polynomial(c(e,d,c,b,a))
solutions<-polyroot(c(e,d,c,b,a))##solutions of the fourth degree polynomial

reale<-matrix(c(0),4,1)#initialization of the matrix "reale" 

v<-matrix(c(solutions[4],solutions[3],solutions[2],solutions[1]))#roots of a quadratic function,arranged  in a reverse order
    
h<-matrix(c(0),4,1)#initialization of the matrix "h"  
k<-matrix(c(0),4,1)#initialization of the matrix "k"

for(i in 1:4){
   reale[i]<-Re(v[i])# we take the real part of the roots
              linearp<-matrix(c(reale[i],1),nrow=2)
              k[i]<-kurt(Z%*%linearp)###we calculate the kurtosis
   k<-kurt(Z%*%linearp)###we calculate the kurtosis
   h[i]<-k ###we put the kurtosis into the vector h
  }
 
M<-max(h)#this is the largest kurtosis
m<-min(h)#this is the smallest kurtosis


if(maxmin=="MAX") {#if we are interested in the projections maximising kurtosis...
       kurtMAX<<-M

         for(i in 1:4){
                  if(h[i]==M){
                              kurtMAX<<-M
              linearMAX<<-data.matrix(sqrtm(solve(cov(data)*(n-1)/n)))%*%rbind(reale[i],1)
              projectionMAX<<-data.matrix(data)%*%linearMAX
                        }
                       }

#####print("maximal kurtosis") 
#####print(M)##the maximum value of kurtosis
#####print("Linear MAX")
#####print(linearMAX)
#####print("projectionMAX")
#####print(projectionMAX)

multi_return1 <- function() {
         my_list1 <- list("maximal kurtosis" = M, "coefficients of the projection maximising kurtosis" = linearMAX, "projection with maximal kurtosis" = projectionMAX)
         return(my_list1)
         
       }
       b<-multi_return1()
       return(b)
       
       
       
    

}
#####}



else if(maxmin=="MIN") {#if we are interested in the projections minimising kurtosis...

for(i in 1:4){
  if(h[i]==m){
              kurtMIN<<-m

linearMIN<<-data.matrix(sqrtm(solve(cov(data)*(n-1)/n)))%*%rbind(reale[i],1)
projectionMIN<<-data.matrix(data)%*%linearMIN

}
}
#####print("minimal kurtosis") 
#####print(m)##the maximum value of kurtosis
#####print("Linear MIN")
#####print(linearMIN)
#####print("projection MIN")
#####print(projectionMIN)

multi_return2 <- function() {
    my_list2 <- list("minimal kurtosis" = m, "coefficients of the projection minimising kurtosis" = linearMIN, "projection with minimal kurtosis" = projectionMIN)
    return(my_list2)
    }
  c<-multi_return2()
  return(c)

}
      }
