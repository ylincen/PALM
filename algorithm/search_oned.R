# This function can be used for constructing 1D MDL histogram, and also for 
#    searching one certain dimension of multivariate data, to construct multi-D MDL histogram

# Input: 
# x: one-d data
# Kmax = maximum number of bins
# eps: granularity of pre-discretization
# which_dim: default 1

# bin: object with the following attributes 
## index: indices of data points within this bin
## max_label: the maximum index of all bins
## done: default FALSE, whether this cell is done with further splitting
## boundary: boundaries


# return_option: 1 = data frame with [bins(left, right), indices], 2 = cut points only, 
#                3 = data frame + dynamic programming matrix + backtrace history + regret

# min_number_points = minimum number of data points to start the search, default 2

# global: default FALSE, whether we use global MDL score or local MDL score
# regret_approximate_SCCI: default T, whether to use the function in SCCI package to calculate the regret.
# eps_for_code_length: default F, whether to include eps in the calculation of probabiliy and code length of data.

# N: global sample size: if not speficied, N <- n. Default NULL.
require(SCCI) # used for calculating regret
require(dplyr)

require(Rcpp)
sourceCpp("algorithm/PALM_Rcpp/dynamic_programming.cpp")
sourceCpp("algorithm/PALM_Rcpp/backtrace.cpp")
search_oned = function(x, Kmax, eps, bin, which_dim = 1,
                       return_option = 1,
                       min_number_points = 2,
                       global = F, regret_approximate_SCCI = T,
                       eps_for_code_length = T, N = NULL, max_label = 1,
                       dims_splitted, candidate_cuts){
  
  ################################################################
  ####################Initilizing and chekcing####################
  ################################################################
  
  # used in functions "get_vols" and "generaete_candidate_cuts"
  # in generate_candidate_cuts: add eps to the last cut point to make sure the last
  #     interval is close on both sides
  # in get_vols: deduct eps from the last cut points to get correct vols
  correct_for_last = T
  
  # initialise some parameters
  n = length(x)
  bin_attr = attributes(bin)
  
  # get index
  index = bin_attr$index
  
  # initialize ranges 
  ranges = bin[seq(2,length(bin),2)] - 
    bin[seq(1,length(bin)-1,2)]
    
  ranges[which_dim] = 1 # exclude the ranges of the dimension being searched now
  vols_other_dims = prod(ranges)
  
  num_dim = length(ranges)
  if(global){
    if(is.null(N)){
      stop("Specify the global sample size in the global mode!")
    }
  }
  
  ################################################################
  ####################Generate cnaidadte cuts#####################
  ################################################################
  
  
  # generate candidate cuts
  # if(length(unique(attr(bin, "boundary")[,which_dim])) > 2){
  #   C_til = generate_candidate_cuts(max = attr(bin, "boundary")[2,which_dim],
  #                                   min = attr(bin, "boundary")[1,which_dim],
  #                                   eps = eps, option = 1,
  #                                   correct_for_last = correct_for_last)
  # } else{
  #   browser()
  #   C_til = generate_candidate_cuts(max = max(x), min = min(x), eps = eps, option = 1,
  #                                   correct_for_last = correct_for_last)
  # }
  if(which_dim %in% dims_splitted){
    # use below, unless the last element of refined candidate cuts are the last element of 
    # candidate_cuts, but we want it to be the last element of C_til
    C_til = candidate_cuts[candidate_cuts >= bin_attr$boundary[,which_dim][1] &
                             candidate_cuts <= bin_attr$boundary[,which_dim][2]]
    C_til_ori = C_til # needed for CL model later
  } else {
    C_til = candidate_cuts
    C_til_ori = C_til[C_til >= min(x)-eps & C_til <= max(x)+eps] # needed for CL model later
  }
  
  
  
  C_0 = C_til[1]
  C_til = C_til[-1] # exculde the left boundary as it is not needed in the dynamic programming search
  
  # refine candiddate cuts
  refined_res = refine_candidate_cuts(x = x, candi_cuts = C_til, left_excluded = T)
  C_til = refined_res$refined_candidate_cuts
  
  if(which_dim %in% dims_splitted){
    C_til = C_til
  } else{
    if(C_til[1] <= min(x)){
      C_0 = C_til[1]
      C_til = C_til[2:length(C_til)]
    }
    
    # I do not remember what the code below does; 
    if(length(C_til) > 1){
      if(C_til[length(C_til)-1] > max(x)){
        C_til = C_til[1:(length(C_til)-1)]
      }  
    }
  }
  
  # return if C_til has nothing to search
  if(length(C_til) < 2){
   attr(bin, "previous_dim") = which_dim
   attr(bin,"done") = T
    if(return_option == 1){
      if(which_dim %in% dims_splitted){
        return(list(bin))
      } else{
        bin[(2*which_dim-1):(2*which_dim)] = c(C_0, C_til)
        attr(bin, "boundary")[,which_dim] = c(C_0, C_til)
        return(list(bin))
      }
    } else {
      #stop("error: invalid return_option in search_oned.R")
      return(NA)
    }
  }
  
  # check min_number_points and check x's range
  if(n <= min_number_points | (max(x) - min(x) < 2 * eps)){
    attr(bin, "done") = T
    if(return_option == 1){
      if(which_dim %in% dims_splitted){
        return(list(bin))
      } else{
        if(C_til[1] > min(x)){
          outer_boundary_small = C_0
        } else {
          outer_boundary_small = C_til[1]
        }
        
        if(C_til[length(C_til) - 1] > max(x)){
          outer_boundary_big = C_til[length(C_til) - 1]
        } else{
          outer_boundary_big = C_til[length(C_til)]
        }
        
        outer_boundary_vec = c(outer_boundary_small, outer_boundary_big)
        
        bin[(2*which_dim-1):(2*which_dim)] = outer_boundary_vec
        attr(bin, "boundary")[,which_dim] = outer_boundary_vec
        return(list(bin))
      }
      
    } else{
      return(NA)
      #stop("error: invalid return_option in search_oned.R")
    }
  }
  
  
  #C_til[length(C_til)] = C_til[length(C_til)] - eps
  ################################################################
  ####################Dynamic programming#########################
  ################################################################
  
  # get counts
  #ne = get_counts(x, C_til)
  ne = refined_res$counts
  
  # correct C_til back
  C_til_bounding = C_til # for get local indices, so that we need to make sure the last interval is close on both sides.
  # if(correct_for_last){
  #   C_til[length(C_til)] = C_til[length(C_til)] - eps
  # }
  
  # dynamic programming
  E = length(C_til)
  Kmax = min(E, Kmax)
  
  
  # The following piece of code is replaced by Rcpp
  
  # B = matrix(rep(NA, E * Kmax), ncol = Kmax)
  # B_which = B
  # 
  # if(global){
  #   n = N
  # }
  # 
  # if(eps_for_code_length){
  #   for(i in 1:E){
  #     if(ne[i] == 0){
  #       B[i,1] = 0
  #     } else{
  #       B[i,1] = - ne[i] * (log2(eps * ne[i]) - log2((C_til[i] - C_0)*n*vols_other_dims))
  #     }
  #   }
  # } else{
  #   for(i in 1:E){
  #     if(ne[i] == 0){
  #       B[i,1] = 0
  #     } else{
  #       B[i,1] = - ne[i] * (log2(ne[i]) - log2((C_til[i] - C_0)*n*vols_other_dims))
  #     }
  #   }
  # }
  # 
  # for(k in 2:Kmax){ # Note that Kmax and E will always be larger than 2
  #   # ... because we checked before
  #   for(i in k:E){ # position of the rightmost cut point
  #     minL = Inf
  #     which_minL = NA
  #     for(j in (k-1):(i-1)){ # position of the second right most cut point
  #       if(eps_for_code_length){
  #         if(ne[i] - ne[j] == 0){
  #           candiL = B[j,k-1]
  #         } else{
  #           candiL = - (ne[i] - ne[j]) * (
  #             log2( eps * (ne[i] - ne[j]) ) - 
  #               log2( (C_til[i] - C_til[j]) * n * vols_other_dims )
  #           ) + B[j,k-1]
  #         }
  #       } else{
  #         if(ne[i] - ne[j] == 0){
  #           candiL = B[j,k-1]
  #         } else{
  #           candiL = - (ne[i] - ne[j]) * (
  #             log2( (ne[i] - ne[j]) ) - 
  #               log2( (C_til[i] - C_til[j]) * n * vols_other_dims)
  #           ) + B[j,k-1]
  #         }
  #       }
  #       
  #       if(candiL < minL){
  #         minL = candiL
  #         which_minL = j
  #       } 
  #     }
  #     B[i,k] = minL
  #     B_which[i,k] = which_minL
  #   }
  # }

  
  dm_res = dynamicProgramming(ne = ne, eps = eps, Kmax = Kmax, E = E, C_til = C_til,
                     vols_other_dims = vols_other_dims, n = n, 
                     eps_for_code_length = eps_for_code_length,C_0 = C_0)
  
  B = dm_res[[1]]
  B_which = dm_res[[2]]
  
  if(global){
    n = length(x)
  }

  ################################################################
  ####################Regret and CL model#########################
  ################################################################
  
  # get regret (i.e., the parametric complexity)
  R = get_regret(n=n,N=N,Kmax=Kmax,num_sub_region_elsewhere = max_label - 1,
                 regret_approximate_SCCI=regret_approximate_SCCI, global=global)
  
  # get L(M)
  #Lm = get_LM(C_til_ori, Kmax, num_dim)
  Lm = get_LM(candidate_cuts, Kmax, num_dim)
  if(Kmax > E/2){
    Lm[round(E/2):length(Lm)] = Lm[round(E/2)]
  }
  # get MDL score
  total_length = B[E,] + R + Lm

  # backtrace and get the cut points
  mm = which.min(total_length) # optimal number of bins
  
  breakss = rep(0, mm)
  EE = E
  if(mm == 1){
    #attr(bin,"dims_searched") = c(attr(bin,"dims_searched"), which_dim)
    attr(bin, "previous_dim") = which_dim
    if(return_option == 1){
      # if(which_dim %in% dims_splitted){
      #   return(list(bin))
      # } else{
      #   # bin[(2*which_dim-1):(2*which_dim)] = c(min(x),max(x)+eps)
      #   # attr(bin, "boundary")[,which_dim] = c(min(x),max(x)+eps)
      #   return(list(bin))
      # }
      
      if(which_dim %in% dims_splitted){
        return(list(bin))
      } else{
        if(C_til[1] > min(x)){
          outer_boundary_small = C_0
        } else {
          outer_boundary_small = C_til[1]
        }
        
        if(C_til[length(C_til) - 1] > max(x)){
          outer_boundary_big = C_til[length(C_til) - 1]
        } else{
          outer_boundary_big = C_til[length(C_til)]
        }
        
        outer_boundary_vec = c(outer_boundary_small, outer_boundary_big)
        
        bin[(2*which_dim-1):(2*which_dim)] = outer_boundary_vec
        attr(bin, "boundary")[,which_dim] = outer_boundary_vec
        return(list(bin))
      }
      
    } else {
      return(total_length[mm])
      #stop("error: invalid return_option in search_oned.R")
    }
  }
  
  #Following code replaced by Rcpp because when B becomes large matrix, R code will become slow.
  breakss = backtrace(B_which = B_which, EE = EE - 1, mm = mm)
  breakss = breakss[-1]
  breakss = breakss + 1 # correct the indices back as C++ using 0 but R use 1;
  breaks = c(C_0, C_til[breakss],C_til[length(C_til)])
  
  ################################################################
  ####################Return the a list of bins###################
  ################################################################
  
  # get indices
  indices = list()
  breaks[length(breaks)] = breaks[length(breaks)] + eps
  for(i in 1:(length(breaks)-1)){
    indices[[i]] = which(x < breaks[i+1] & x >= breaks[i])
  }
  breaks[length(breaks)] = breaks[length(breaks)] - eps

  # get boundaries
  boundaries = rbind(breaks[1:(length(breaks)-1)],
                     breaks[-1])
  
  # update labels
  labels = c(bin_attr$label, max_label + 1:(mm-1))

  # get the attributes of bins
  bins_list = list()
  for(i in 1:length(labels)){
    l = labels[i]
    
    
    new_boundary = attr(bin,"boundary")
    new_boundary[ ,which_dim] = boundaries[,i]
    
    bins_list[[i]] = as.vector(new_boundary)
    
    attr(bins_list[[i]], "boundary") = new_boundary
    
    
    attr(bins_list[[i]], "index") = attr(bin, "index")[indices[[i]]]
    attr(bins_list[[i]], "done") = FALSE
    attr(bins_list[[i]], "label") = labels[i]
    
    attr(bins_list[[i]], "neighbouring_bins") = 0
    attr(bins_list[[i]], "previous_dim") = which_dim
  }
  if(return_option == 1){
    return(bins_list)  
  } else {
    #stop("error: invalid return_option in search_oned.R")
    return(total_length[mm])
  }
}
  
  