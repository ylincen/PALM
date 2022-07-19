multi_hist_merging = function(bins, eps, force_not_to_merge=F){
  # This functions excutes the merging phase of our discretization algorithm
  
  # res is the splititng res by multi_hist_splitting
  # eps is the data precision;
  # output: 
  # g: a graph indicates the merging results;
  # stochasticComp: ..
  # counts and vols: averaged counts and vols as if these sub-regions are merged with neighbouring sub-regions
  
  
  cuts_mat = matrix(unlist(bins), byrow=T, ncol=length(bins[[1]]))
  counts = sapply(bins, function(x){length(attr(x, "index"))})
  
  vols = cuts_mat[,seq(2,ncol(cuts_mat),2)] - cuts_mat[,seq(1,ncol(cuts_mat),2)] 
  
  if(length(dim(vols)) == 0){
    vols = vols # for 1d case
  } else{
    vols = apply(vols, 1, prod)
  }
  #n = nrow(data)
  n = sum(counts)
  
  
  
  
  num_dim = ncol(cuts_mat)/2
  
  
  vols[vols < eps^num_dim] = eps^num_dim
  
  cuts_mat = round(cuts_mat, digits = -log10(eps))
  
  
  require(igraph)
  g = graph(edges = c(), n = nrow(cuts_mat), directed = F)
  R = get_regret(n=n, N=n, Kmax= nrow(cuts_mat), num_sub_region_elsewhere = 0,
                 regret_approximate_SCCI=T, global=T)
  K = length(R)
  #current_CL = -sum(counts * (modified_log(counts * eps^num_dim) - log2(n * vols))) + R[K]
  current_CL = -sum(counts * (log2(counts * eps^num_dim) - log2(n * vols)), na.rm = T) + R[K]
  
  if(force_not_to_merge){
    return(list(g = g, counts = counts, vols = vols, stochasticComp = current_CL, R = R[K]))
  }
  
  # find all neighbouring pairs of sub-regions
  
  #transpose the cuts_mat in order to make row-wise calculation to col-wise calculation
  neighbouring_pairs = NULL
  cuts_mat_reverse = cuts_mat[,rbind(seq(2, 2*num_dim,2), seq(1,2*num_dim,2)) %>% as.numeric()]
  cuts_mat = t(cuts_mat); cuts_mat_reverse = t(cuts_mat_reverse)
  for(i in 1:length(counts)){
    whether_possible = cuts_mat[,i] - cuts_mat_reverse
    
    #whether_within_range = cuts_mat[i,] - cuts_mat[-i,]
    reverse_equal = whether_possible %>% apply(2,function(x){which(abs(x-0) < eps/10)})
    which_col_possible = which(sapply(reverse_equal, length) == 1)
    which_col_possible = which_col_possible[which_col_possible > i]
    if(length(which_col_possible)==0){
      next()
    }
    for(j in 1:length(which_col_possible)){
      dim_reverse_equal = reverse_equal[[which_col_possible[j]]]/2 %>% ceiling()
      dim_to_consider_within_range = setdiff(1:num_dim, dim_reverse_equal)
      rows_to_consider_within_range = rbind(dim_to_consider_within_range*2 - 1, 
                                            dim_to_consider_within_range*2) %>% as.numeric()
      
      signs_within_range = whether_possible[rows_to_consider_within_range,which_col_possible[[j]]] %>% 
        sign() %>% unname()
      if(isTRUE(all.equal(signs_within_range, rep(c(-1,1), length(signs_within_range)/2)))){
        neighbouring_pairs = cbind(neighbouring_pairs, c(i,which_col_possible[[j]]))
      }
    }
  }
  
  # in case no neighbouring pairs exist
  if(is.null(neighbouring_pairs)){
    return(list(g = g, counts = counts, vols = vols, stochasticComp = current_CL, R = R[K]))
  }
  cuts_mat = t(cuts_mat)
  
  
  iiter = 1
  #-sum(counts * (modified_log(counts) + num_dim* log2(eps) - log2(n * vols))) + R[K]
  
  #merge(cuts_mat, data, 0.01)
  
  while(T){
    new_CL = rep(Inf, ncol(neighbouring_pairs))
    old_g = g
    for(i in 1:ncol(neighbouring_pairs)){
      if(components(old_g)$membership[neighbouring_pairs[1,i]] ==
         components(old_g)$membership[neighbouring_pairs[2,i]]){
        next()
      }
      g = add_edges(old_g, neighbouring_pairs[,i])
      group_members = which(components(g)$membership == components(g)$membership[neighbouring_pairs[1,i]])
      
      counts_if_merge = counts
      vols_if_merge = vols
      
      
      counts_if_merge[group_members] = mean(counts_if_merge[group_members])
      vols_if_merge[group_members] = mean(vols_if_merge[group_members])
      
      # new_CL[i] = -sum(counts_if_merge * (modified_log(counts_if_merge * eps^num_dim) - log2(n * vols_if_merge))) +
      #   R[K-1]
      new_CL[i] = -sum(counts_if_merge * (log2(counts_if_merge * eps^num_dim) - log2(n * vols_if_merge)), na.rm = T) +
        R[K-1]
    }
    if(min(new_CL, na.rm = T) < current_CL){
      which_min_new_CL = which.min(new_CL)
      g = add_edges(old_g, neighbouring_pairs[,which_min_new_CL])
      group_members = which(components(g)$membership == components(g)$membership[neighbouring_pairs[1,which_min_new_CL]])
      counts[group_members] = mean(counts[group_members])
      vols[group_members] = mean(vols[group_members])
      
      current_CL = min(new_CL, na.rm = T)
      K = K-1
      iiter = iiter + 1
    } else{
      g = old_g
      #print("merging is done")
      break()
    }
  }
  return(list(g = g, counts = counts, vols = vols, stochasticComp = current_CL, R = R[K]))
}