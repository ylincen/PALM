require(dplyr)
# eps is not used here!!
initialize_bin = function(data, candidate_cuts_list, eps = NULL){

  
  if(dim(data) %>% is.null){
    boundary = t(t(round(c(candidate_cuts_list[[1]][1], 
                           candidate_cuts_list[[1]][length(candidate_cuts_list[[1]])]), 
                         digits = dig))) # make it a one-col matrix
    if(min(data)==max(data)){
      stop("error: min(data) is equal to max(data)") # in case all data points have the same value
    }
  } else{
    boundary = matrix(rep(NA, 2 * ncol(data)), ncol = ncol(data))
    for(i in 1:ncol(data)){
      boundary[1,i] = candidate_cuts_list[[i]][1]
      boundary[2,i] = candidate_cuts_list[[i]][length(candidate_cuts_list[[i]])]
    }
  }

  bin = as.vector(boundary)
  
  attr(bin, "boundary") = boundary
  if(dim(data) %>% is.null){
    attr(bin, "index") = 1:length(data)
  } else{
    attr(bin, "index") = 1:nrow(data)
  }
  attr(bin, "neighbouring_bins") = c()
  attr(bin, "dims_searched") = c()
  attr(bin, "done") = FALSE
  attr(bin, "label") = 1
  attr(bin, "previous_dim") = NULL
  
  return(bin)
}