get_which_dim = function(num_dim, bin, option = c("alternative","random"),
                         start_dim = 1){
  if(option == "alternative"){
    previous_dim = attr(bin, "previous_dim")
    
    if(length(previous_dim)==0){
      return(start_dim)
    }
    
    if(previous_dim == num_dim){
      return(1)
    } else{
      return(previous_dim + 1)
    }
  } else if(option == "random"){
    previous_dim = attr(bin, "previous_dim")
    
    if(length(previous_dim)==0){
      return(sample(1:num_dim, size = 1))
    }
    
    candidate_dims = setdiff(1:num_dim, previous_dim)
    if(length(candidate_dims) == 0){
      stop("Error in get_which_dim.R: data must be at least 2D!")
    }
    selected_dim = sample(candidate_dims, size = 1)
    return(selected_dim)
  } else {
    stop("Error in get_which_dim.R: option must be 'alternative' or 'random'!")
  }
}