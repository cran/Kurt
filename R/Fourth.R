Fourth <-
function(data,type,shape,feature){###matrices of fourth moments or fourth multivariate cumulants
#The function recalls the function Fourth4 and the function Cum4.
#Fourth moment of a data matrix, that is a d^2 x d^2 matrix which includes
#all fourth moments , where d is the number of variables
#type=0 is the ordinary fourth momenti
#type=1 is the centered fourth moment
#type=2 is the standardized fourth moment


#for the fourth multivariate cumulant
#type=0 is the ordinary fourth moment
#type=1 uses original or centered data
#type=2 uses  standardized data

#shape= may be "square" or "rectangular". If "shape" equals "rectangular", we work with a cokurtosis matrix
#feature= may be "moment" or "cumulant". If feature is "moment" the function computes the fourth moment of a data matrix,
#that is a  d^2 x d^2 matrix which includes all fourth moments, where d is the number of variables.
#The function recalls the function Fourth4  
#If feature is "cumulant", the function computes the fourth multivariate cumulants.
#The function recalls the  function Cum4.

if (feature== "moment"){
Fourth4(data,type,shape)
}

else if (feature=="cumulant"){
Cum4(data,type,shape)
}

}
