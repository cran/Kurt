ExtKur <-function(data,iterations,maxmin){
#Kurtosis-based projection pursuit. Data projection with either maximal or minimal
#kurtosis.The kurtosis of a distribution is intended as the fourth standardized
#moment of the distribution itself. ExtKur calls the functions "optik" and
#"ExtKurBiv" to compute the starting values and the updated values, respectively.
##Parameters in input are
##data = data matrix where rows and columns represent units and variables.
##iterations = number of required iterations. It must be a number greater or equal 1
##maxmin is the choice to either maximize or minimize kurtosis
##Values in output: 
##linear = a vector of coefficients. (linearMAX,linearMIN)
##projection = vector of projected data.(projectionMAX,projectionMIN)
##kurt= extreme kurtosis attainable by a data projection. (kurttMAX, kurtMIN)
  kurMAX<-NULL
  dMAX<-NULL
  pMAX<-NULL
  linearMAX<-NULL
  kurtMAX<-NULL
   kurttMAX<-NULL
  projectionMAX<-NULL
  kurMINbis<-NULL
  dMINbis<-NULL
  linearMIN<-NULL
  kurtMIN<-NULL
  projectionMIN<-NULL
  
  rm("projectionMIN")
  rm("kurtMIN")
  rm("linearMIN")
  rm("dMINbis")
  rm("kurMINbis")
  rm("projectionMAX")
  rm("kurttMAX")
   rm("linearMAX")
  rm("kurtMAX")
  rm("dMAX")
  rm("kurMAX")
  rm("pMAX")
  if(iterations<0) {
    message("Error, iterations must be a number greater than zero")
  }
  if(iterations==0) {
    message ("Error, iterations must be a number greater than zero")
  }
data<-data.matrix(data)
if(iterations>0) {

n<-nrow(data)
d<-ncol(data)

O<-optik(data)#generates the starting values


if(maxmin=="MAX"){#we'll find data projection with maximal kurtosis

M<-kurMAX  ##starting value of maximal kurtosis
v<-dMAX ##starting value of linear
pMAX<-pMAX



rm(kurMAX,envir = as.environment(1), inherits = FALSE)
rm(pMAX,envir = as.environment(1), inherits = FALSE)
rm(dMAX,envir = as.environment(1), inherits = FALSE)


for(i in 1:iterations){

for(j in 1:d){
  y<-data[,j]#j-th column of the data matrix
  v[j]<-0
  EXTKURBIV<-ExtKurBiv(cbind(data%*%v,y),"MAX")
  k<-kurtMAX###kurtosis of the new linear combination
  v[j]<-linearMAX[2,1]/linearMAX[1,1]
  if(k>M) ###{#if the new kurtosis is greater than the current maximum...
M<-k#...make the new kurtosis the current maximum
kurttMAX<<-k#...set the new kurtosis equal to kurtMAX
linearMAX<<-v#set the vector of coefficients equal to linearMAX
projectionMAX<<-data%*%v#project the data onto the direction of v

     }
}

multi_returnExtKur_1 <- function() {
  my_listExtKur_1 <- list("kurt" = kurttMAX,"linear"=linearMAX,"projection"=projectionMAX )
  return(my_listExtKur_1)
  
}
b<-multi_returnExtKur_1()
return(b)



}




else if(maxmin=="MIN"){

###this follows if maxmin==MIN
m<-kurMINbis  ##starting value of minimal kurtosis
v<-dMINbis ##starting value of linear
pMINbis<-pMINbis
x<-data%*%v #starting value of projection


for(i in 1:iterations){

for(j in 1:d){
y<-data[,j]#j-th column of the data matrix
v[j]<-0
ExtKurBiv(cbind(data%*%v,y),"MIN")
k<-kurtMIN###kurtosis of the new linear combination
v[j]<-linearMIN[2,1]/linearMIN[1,1]
 if(k<m) #####{#if the new kurtosis is less than the current minimum...
 m<-k#...make the new kurtosis the current minimum
 kurtMIN<<-k#...set the new kurtosis equal to kurtMIN

linearMIN<<-v#set the vector of coefficients equal to linearMIN

projectionMIN<<-data%*%v#project the data onto the direction of v

             }
}

rm(kurMINbis,envir = as.environment(1), inherits = FALSE)
rm(dMINbis,envir = as.environment(1), inherits = FALSE)
rm(pMINbis,envir = as.environment(1), inherits = FALSE)

multi_returnExtKur_2 <- function() {
  my_listExtKur_2 <- list("kurt" = kurtMIN,"linear"=linearMIN,"projection"=projectionMIN )
  return(my_listExtKur_2)
  
}
b<-multi_returnExtKur_2()
return(b)




}
}

}
