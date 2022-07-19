get_counts = function(x, C_til){
  counts = sapply(C_til, function(y){
    sum(x < y) # since our bins are assumed to be [ ) except for the last one
  })
}


# NOT USED ANYMORE
# get_vols = function(C_0, C_til, ranges, correct_for_last){
#   # Input:
#   # ranges: vector of ranges of other dimensions
#   
#   if(correct_for_last){
#     C_til[length(C_til)] = C_til[length(C_til)] - eps
#   }
#   vols = prod(ranges) * C_til # 'ranges' is set to 1 if 
# }

L_int = function(n, k0 = 2.865064, max_iter = 1000, print_iter = F){
  # The integer code for n has the code length log2(k0) + log2(n) + log2 log2 (n) + ...
  # k0 is used to make the Kraft's inequality equal. See Grunwald MDL book page 100.
  res = log2(k0)
  tmp = log2(n)
  for(i in 1:max_iter){
    if(tmp < 0){
      if(print_iter){
        cat(i, "iterations used! \n")
      }
      return(res)
    } else{
      res = res + tmp
      tmp = log2(tmp)
    }
  }
  stop("error: max_iter reached.")
}

get_regret = function(n, N, Kmax, num_sub_region_elsewhere, regret_approximate_SCCI, global){
  if(regret_approximate_SCCI){
    if(global){
      R = sapply(1:Kmax, function(i){
        regret(N, i + num_sub_region_elsewhere)
      })
    } else {
      R = sapply(1:Kmax, function(i){
        regret(n, i)
      })
    }
  } else {
    stop("error: feature deprecated in get_regret in utils.R")
    # Tested by comparing the results of SCCI::regret and my 
    # own slow implemention calculate_COMP.R, and the difference is 
    # no more than 0.003
  }
  return(R)
}
