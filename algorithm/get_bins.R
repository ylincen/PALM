# This script collects all functions that get bins (bins like PALM) from 
#    discretization methods that we compare: PALM, iterative MDL, seperate MDL
#    maybe later: DET, DSP (discrepancy sequential partition, NIPS 2016); 

get_bins_PALM = function(data, eps, Kmax, start_dim = 1, 
                         visual = F, force_not_to_merge = F, 
                         return_mat = T){
  # get bins and merged bins
  bins = search_multid(data = data,eps = eps, Kmax = Kmax,start_dim = start_dim)
  if(length(bins) != 1){
    merge_res = multi_hist_merging(bins, eps, 
                                   force_not_to_merge = force_not_to_merge)
  } else {
    merge_res = multi_hist_merging(bins, eps, 
                                   force_not_to_merge = T)
  }
  
  # visualize
  if(visual){
    cut_mat = matrix(unlist(bins),byrow = T,ncol = length(bins[[1]]))
    visualize_twod_result(cut_mat, merge_res, eps) %>% print()
  }
  
  # equip the bins with densities
  merged_bins = get_densities_for_bins(bins, merge_res, n = nrow(data)) 
  
  if(return_mat){
    merged_bins_mat = unlist(merged_bins) %>% 
      matrix(ncol=length(merged_bins[[1]]), byrow = T)
    merged_bins_mat = cbind(merged_bins_mat, 
                            sapply(merged_bins, attr, "density"))
    return(merged_bins_mat)
  } else {
    return(merged_bins)  
  }
}

get_bins_sepMDL = function(data, eps, Kmax, start_dim = 1){
  # get bins for each dimension
  bins = apply(data, 2, function(x){
    search_multid(data = t(t(x)), eps = eps, Kmax = Kmax, 
                  start_dim = start_dim)
  })
  
  # construct the multi-dimensional grid based on the bins of each dimension; 
}

get_bins_mat = function(merged_bins){
  merged_bins_mat = unlist(merged_bins) %>% 
    matrix(ncol=length(merged_bins[[1]]), byrow = T)
  merged_bins_mat = cbind(merged_bins_mat, 
                          sapply(merged_bins, attr, "density"))
  return(merged_bins_mat)
}
