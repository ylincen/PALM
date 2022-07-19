#This is to check whether two intervals share common edges. 

overlap = function(x, y, show_common_edge = F){
  #x and y are considered as OPEN set in R^2
  #x and y are both a vector of length 4, ordered. 
  #show_common_edge controls whether to return the common edges, default FALSE
  

  z = x[c(2,1,4,3)]
  
  if(!any(z == y)){
    return(FALSE)
  } else {
    b = which(z == y)
  }
  
  if(length(b)>1){
    #corners meet
    return(FALSE)
  }

  if(b >= 3){
    if(x[2] <= y[1] | x[1] >= y[2]){
      return(FALSE)
    } else{
      if(show_common_edge){
        return(c(max(x[1],y[1]),y[b], min(x[2],y[2]), y[b]))
      } else {
        return(TRUE)
      }
    }
  } else if(b <= 2){
    if(x[4] <= y[3] | x[3] >= y[4]){
      return(FALSE)
    } else{
      if(show_common_edge){
        return(c(y[b], max(x[3],y[3]), y[b], min(x[4],y[4])))
      } else {
        return(TRUE)
      }
    }
  }
}


