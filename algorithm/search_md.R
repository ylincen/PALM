search_multid = function(data, eps, Kmax,
                         global = F, min_number_points = 2, 
                         eps_for_code_length = T,
                         return_option = 1, max_iter = 300,
                         start_dim = 1, write_to_file_intermediate_results=F,
                         dim_choose){
  # generate candidate cuts for each dimension
  if(length(eps) == 1){
    eps = rep(eps, ncol(data))
  } 
  if(length(eps) != ncol(data)){
    stop("error search_md.R: eps should be either of length 1 or the same
         length as the ncol(data)")
  }
  
  candidate_cuts_list = list()
  for(i in 1:ncol(data)){
    x = data[,i, drop=F]
    candidate_cuts_res = generate_candidate_cuts(max=max(x), min=min(x), 
                                                 eps=eps[i], option = 1, x=x)
    data[,i] = candidate_cuts_res$x
    candidate_cuts_list[[i]] = candidate_cuts_res$cuts
  } 
  
  # initialise a bin
  initial_bin = initialize_bin(data = data, candidate_cuts_list = candidate_cuts_list)
  
  # dims splitted at least once (for detecting outer boundaries)
  dims_splitted = c()
  
  # iteratively split each bin
  if(write_to_file_intermediate_results){
    df_to_file_nrow = data.frame(matrix(ncol=2, nrow=0))
    colnames(df_to_file_nrow) = c("nrow","iter")
    
    df_to_file_bins = data.frame(matrix(nrow=0, ncol=6))
    colnames(df_to_file_bins) = c("iter", "bin_index", "left1", 
                                  "right1", "left2", "right2") 
  }
  old_bins_list = list(initial_bin)
  for(iter in 1:max_iter){
    
    if(write_to_file_intermediate_results){
      to_file_res_this_iter = c(length(old_bins_list), iter)
      df_to_file_nrow[nrow(df_to_file_nrow)+1,] = to_file_res_this_iter
    }
    
    new_bins_list = list()
    finished = T
    for(i in 1:length(old_bins_list)){
      # if(iter==2 & i==3){
      #   browser()
      # }
      if(write_to_file_intermediate_results){
        df_to_file_bins[nrow(df_to_file_bins) + 1, ] = 
          c(iter, i, old_bins_list[[i]])
      }

      local_data = data[attr(old_bins_list[[i]], "index"), ,drop=F]
      
      # Control when to stop
      if(attr(old_bins_list[[i]], "done")){
        new_bins_list = append(new_bins_list, list(old_bins_list[[i]]))
        next
      } else if(
          isTRUE(
            all.equal(
              sort(attr(old_bins_list[[i]], "dims_searched")), # dims_searched means that dimensions searched but no new cut points are added
              1:ncol(data)
            )
          )
        ){
        attr(old_bins_list[[i]], "done") = TRUE
        new_bins_list = append(new_bins_list, list(old_bins_list[[i]]))
        next
      } else if(length(attr(old_bins_list[[i]], "index")) == 0){
        attr(old_bins_list[[i]], "done") = TRUE
        new_bins_list = append(new_bins_list, list(old_bins_list[[i]]))
        next
      } else {
        finished = F
      }
      
      # which dimension to split
      # cat("iter, i: ", iter, i, "\n")
      # if(iter == 3 & i == 37){
      #   browser()
      # }
      which_dim = get_which_dim(ncol(local_data),
                                old_bins_list[[i]],
                                option = "alternative",
                                start_dim = start_dim)
      if(dim_choose == "greedy"){
        which_dim = 
          get_which_dim_greedy(local_data = local_data, Kmax=Kmax, 
                               eps=eps, old_bins_list = old_bins_list,
                               return_option = 100, 
                               min_number_points = min_number_points, 
                               global = global, regret_approximate_SCCI = T, 
                               eps_for_code_length = eps_for_code_length, 
                               N = NULL, max_label = length(old_bins_list), 
                               dims_splitted = dims_splitted, 
                               candidate_cuts_list = candidate_cuts_list,
                               start_dim = start_dim, i=i)
      }
      
      
      # search on that dimension
      bins_list = search_oned(x = local_data[,which_dim], Kmax = Kmax, 
                              eps = eps[which_dim], bin = old_bins_list[[i]], 
                              which_dim = which_dim, 
                              return_option = return_option, 
                              min_number_points = min_number_points, 
                              global = global, regret_approximate_SCCI = T, 
                              eps_for_code_length = eps_for_code_length, 
                              N = NULL, max_label = length(old_bins_list), 
                              dims_splitted = dims_splitted, 
                              candidate_cuts = candidate_cuts_list[[which_dim]])
      
      new_bins_list = append(new_bins_list, bins_list)
      
      if(sapply(new_bins_list, is.null) %>% any){
        browser()
      }
      if(any(sapply(new_bins_list, length) == 1)){
        browser()
      }
    }
    
    if(finished){
      break
    } else{
      dims_splitted = c(dims_splitted, which_dim)
      old_bins_list = new_bins_list
    }
  }
  
  if(write_to_file_intermediate_results){
    write.csv(df_to_file_nrow, file="PALM_intermetiate_results_nrow.csv", 
              row.names=F)
    write.csv(df_to_file_bins, file="PALM_intermetiate_results_bins.csv", 
              row.names=F)
  }
  return(new_bins_list)
}


get_which_dim_greedy = function(local_data, Kmax, 
                                eps, old_bins_list,
                                return_option, min_number_points, 
                                global, regret_approximate_SCCI, 
                                eps_for_code_length,
                                N, max_label, 
                                dims_splitted, 
                                candidate_cuts_list, start_dim, i){
  total_lengths = rep(0, ncol(local_data))

  for(which_dim in 1:ncol(local_data)){
    oned_res = search_oned(x = local_data[,which_dim], Kmax = Kmax, 
                           eps = eps[which_dim], bin = old_bins_list[[i]], 
                           which_dim = which_dim, 
                           return_option = return_option, 
                           min_number_points = min_number_points, 
                           global = global, regret_approximate_SCCI = T, 
                           eps_for_code_length = eps_for_code_length, 
                           N = NULL, max_label = length(old_bins_list), 
                           dims_splitted = dims_splitted, 
                           candidate_cuts = candidate_cuts_list[[which_dim]])
    if(is.na(oned_res)){
      total_lengths[which_dim] = Inf
    } else {
      total_lengths[which_dim] = oned_res
    }
  }
  return(which.min(total_lengths))
}