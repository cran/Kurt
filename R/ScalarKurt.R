ScalarKurt <-
function(data,feature,type,prob){

#data is a data matrix 
#feature may be moment or cumulant
#type may be Mardia or Koziol
#prob may be lower if probabilities are P[X <= x], upper if P[X > x], twoside if probabilities are lower and upper
###For Koziol kurtosis only the upper tail is meaningful

##values in output: statistic and pvalue 
  statistic<-NULL
  pvalue<-NULL
  pvalue1<-NULL
  pvalue2<-NULL
  M<-NULL
  K4<-NULL
  rm("statistic")
  rm("pvalue")
  rm("pvalue1")
  rm("pvalue2")
  rm("M")
  rm("K4")
  
  

if (feature=="moment") {

if (type=="Mardia") {###trace of fourth standardized moment 
index<-sum(diag(Fourth4(data,2,"square")))
statistic<<-(index-ncol(data)*(ncol(data)+2))/sqrt((8*ncol(data)*(ncol(data)+2))/nrow(data))


#print("statistic")
#print(statistic)

if(prob=="lower") {
pvalue<<-pnorm(statistic, mean = 0, sd = 1,lower.tail=TRUE)###if lower.tail=TRUE (default) probability is P[X < x] otherwise, P[X > x].


#print("pvalue")
#print(pvalue)

multi_returnScalarKurt <- function() {
  my_listScalarKurt <- list("statistic" = statistic, "pvalue"= pvalue)
  return(my_listScalarKurt)
  
}
bb<-multi_returnScalarKurt()
return(bb)
}

else if (prob=="upper") {
pvalue<<-pnorm(statistic, mean = 0, sd = 1,lower.tail=FALSE)###if lower.tail=TRUE (default) probability is P[X < x] otherwise, P[X > x].

#print("pvalue")
#print(pvalue)

multi_returnScalarKurt_2 <- function() {
  my_listScalarKurt_2 <- list("statistic" = statistic, "pvalue"= pvalue)
  return(my_listScalarKurt_2)
  
}
bb_2<-multi_returnScalarKurt_2()
return(bb_2)

}

else if (prob=="twoside") {
pvalue1<<-pnorm(statistic, mean = 0, sd = 1,lower.tail=TRUE)###if lower.tail=TRUE (default) probability is P[X < x] otherwise, P[X > x].
pvalue2<<-pnorm(statistic, mean = 0, sd = 1,lower.tail=FALSE)###if lower.tail=TRUE (default) probability is P[X < x] otherwise, P[X > x].

#print("pvalue1")
#print(pvalue1)
#print("pvalue2")
#print(pvalue2)

multi_returnScalarKurt_3 <- function() {
  my_listScalarKurt_3 <- list("statistic" = statistic, "pvalue1"= pvalue1, "pvalue2"= pvalue2)
  return(my_listScalarKurt_3)
  
}
bb_3<-multi_returnScalarKurt_3()
return(bb_3)

}

rm(M,envir = as.environment(1), inherits = FALSE)

}

else if (type=="Koziol") {
index<-sum(diag(Fourth4(data,2,"square")%*%t(Fourth4(data,2,"square"))))

statistic<<-(index-3*ncol(data)*(ncol(data)+2))/sqrt((288*ncol(data)*(ncol(data)+2))/nrow(data))

#print("statistic")
#print(statistic)


if(prob=="lower") {
pvalue<<-1-pnorm(statistic, mean = 0, sd = 1,lower.tail=TRUE)###if lower.tail=TRUE (default) probabilities are P[X < x] otherwise, P[X > x].


#print("pvalue")
#print(pvalue)

multi_returnScalarKurt_4 <- function() {
  my_listScalarKurt_4 <- list("statistic" = statistic, "pvalue"= pvalue)
  return(my_listScalarKurt_4)
  
}
bb_4<-multi_returnScalarKurt_4()
return(bb_4)
}

else if (prob=="upper") {
pvalue<<-1-pnorm(statistic, mean = 0, sd = 1,lower.tail=FALSE)###if lower.tail=TRUE (default) probability is P[X < x] otherwise, P[X > x].

#print("pvalue")
#print(pvalue)

multi_returnScalarKurt_5 <- function() {
  my_listScalarKurt_5 <- list("statistic" = statistic, "pvalue"= pvalue)
  return(my_listScalarKurt_5)
  
}
bb_5<-multi_returnScalarKurt_5()
return(bb_5)
}

else if (prob=="twoside") {
pvalue1<<-1-pnorm(statistic, mean = 0, sd = 1,lower.tail=TRUE)###if lower.tail=TRUE (default) probability is P[X < x] otherwise, P[X > x].
pvalue2<<-1-pnorm(statistic, mean = 0, sd = 1,lower.tail=FALSE)###if lower.tail=TRUE (default) probability is P[X = x] otherwise, P[X > x].


#print("pvalue1")
#print(pvalue1)
#print("pvalue2")
#print(pvalue2)

multi_returnScalarKurt_6 <- function() {
  my_listScalarKurt_6 <- list("statistic" = statistic, "pvalue1"= pvalue1, "pvalue2"= pvalue2)
  return(my_listScalarKurt_6)
  
}
bb_6<-multi_returnScalarKurt_6()
return(bb_6)

}


rm(M,envir = as.environment(1), inherits = FALSE)

}
}

  else if (feature=="cumulant") {

 if (type=="Mardia") {

Cum4(data,2,"square")
index<-sum(diag(K4)) ###trace of the fourth standardized cumulant

#print(index)

statistic<<-index/sqrt((8*ncol(data)*(ncol(data)+2))/nrow(data))

#print("statistic")
#print(statistic)

if(prob=="lower") {
pvalue<<-pnorm(statistic, mean = 0, sd = 1,lower.tail=TRUE)###if lower.tail=TRUE (default) probability is P[X < x] otherwise, P[X > x].

#print("pvalue")
#print(pvalue)

multi_returnScalarKurt_7 <- function() {
  my_listScalarKurt_7 <- list("statistic" = statistic, "pvalue"= pvalue)
  return(my_listScalarKurt_7)
  
}
bb_7<-multi_returnScalarKurt_7()
return(bb_7)
}

else if (prob=="upper") {
pvalue<<-pnorm(statistic, mean = 0, sd = 1,lower.tail=FALSE)###if lower.tail=TRUE (default) probability is P[X = x] otherwise, P[X > x].


#print("pvalue")
#print(pvalue)

multi_returnScalarKurt_8 <- function() {
  my_listScalarKurt_8 <- list("statistic" = statistic, "pvalue"= pvalue)
  return(my_listScalarKurt_8)
  
}
bb_8<-multi_returnScalarKurt_8()
return(bb_8)

}

else if (prob=="twoside") {
pvalue1<<-pnorm(statistic, mean = 0, sd = 1,lower.tail=TRUE)###if lower.tail=TRUE (default) probability is P[X = x] otherwise, P[X > x].
pvalue2<<-pnorm(statistic, mean = 0, sd = 1,lower.tail=FALSE)###if lower.tail=TRUE (default) probability is P[X = x] otherwise, P[X > x].

#print("pvalue1")
#print(pvalue1)
#print("pvalue2")
#print(pvalue2)

multi_returnScalarKurt_9 <- function() {
  my_listScalarKurt_9 <- list("statistic" = statistic, "pvalue1"= pvalue1,"pvalue2"= pvalue2)
  return(my_listScalarKurt_9)
  
}
bb_9<-multi_returnScalarKurt_9()
return(bb_9)

}

rm(K4,envir = as.environment(1), inherits = FALSE)

}



else if (type=="Koziol") {
Cum4(data,2,"square")
index<-sum(diag(K4%*%K4)) ###trace of the squares fourth standardized cumulant

statistic<<-(nrow(data)/24)*sum(diag(K4%*%K4))

g<-ncol(data)*(ncol(data)+1)*(ncol(data)+2)*(ncol(data)+3)/24

if(prob=="lower") {
message("ERROR")
}

else if (prob=="upper") {
#print("statistic")
#print(statistic)
  
pvalue<<-1-pchisq(statistic, g, lower.tail=FALSE)###if lower.tail=TRUE (default) probabilities are P[X = x] otherwise, P[X > x].

#print("pvalue")
#print(pvalue)

multi_returnScalarKurt_10 <- function() {
  my_listScalarKurt_10 <- list("statistic" = statistic, "pvalue"= pvalue)
  return(my_listScalarKurt_10)
  
}
bb_10<-multi_returnScalarKurt_10()
return(bb_10)


}

else if (prob=="twoside") {
message("ERROR")
}


}


  }
}

