# get test data for MISE
get_test_data_for_MISE = function(test_data, learn_df, empirical_density, eps, 
                                  exclude_uncovered){
  test_data_for_MISE = test_data
  test_data_for_MISE["which_region_learn"] = rep(0,nrow(test_data_for_MISE))
  
  for(i in 1:nrow(learn_df)){
    indexs = which(test_data_for_MISE[,1] < learn_df[i,2] &
                     test_data_for_MISE[,1] >= learn_df[i,1] &
                     test_data_for_MISE[,2] < learn_df[i,4] &
                     test_data_for_MISE[,2] >= learn_df[i,3])
    test_data_for_MISE$which_region_learn[indexs] = i
  }
  
  synthetic_data_on_boundary_or_outer = filter(test_data_for_MISE, which_region_learn ==0)
  for(i in 1:nrow(learn_df)){
    indexs = which(synthetic_data_on_boundary_or_outer[,1] <= learn_df[i,2] &
                     synthetic_data_on_boundary_or_outer[,1] >= learn_df[i,1] &
                     synthetic_data_on_boundary_or_outer[,2] <= learn_df[i,4] &
                     synthetic_data_on_boundary_or_outer[,2] >= learn_df[i,3])
    synthetic_data_on_boundary_or_outer$which_region_learn[indexs] = i
  }
  
  test_data_for_MISE = test_data_for_MISE %>% as.data.frame()
  test_data_for_MISE$which_region_learn[test_data_for_MISE$which_region_learn == 0] = 
    synthetic_data_on_boundary_or_outer$which_region_learn
  
  
  if(exclude_uncovered){
    test_data_for_MISE = filter(test_data_for_MISE, which_region_learn != 0)  
    learn_den = empirical_density[test_data_for_MISE$which_region_learn] 
    test_data_for_MISE$learn_density = learn_den
    colnames(test_data_for_MISE)[1:2]=c("x","y")
    test_data_for_MISE = test_data_for_MISE[test_data_for_MISE$learn_density>0,]
  } else {
    learn_den = rep(0, nrow(test_data_for_MISE))
    learn_den[test_data_for_MISE$which_region_learn == 0] = 0 # uncovered
    
    covered_indices = (test_data_for_MISE$which_region_learn != 0)
    learn_den[covered_indices] = 
      empirical_density[test_data_for_MISE$which_region_learn[covered_indices]]
    
    test_data_for_MISE$learn_density = learn_den
    colnames(test_data_for_MISE)[1:2]=c("x","y")
  }
  return(test_data_for_MISE)
}


# get test_data_for_MISE (also for plotting)
get_synthetic_data_for_MISE = function(data, learn_df, empirical_density, eps,
                                       exclude_uncovered=T){
  synthetic_data_for_MISE = expand.grid(seq(min(data[,1]),max(data[,1]),by=eps),
                                        seq(min(data[,2]),max(data[,2]),by=eps))
  synthetic_data_for_MISE["which_region_learn"] = rep(0,nrow(synthetic_data_for_MISE))
  
  for(i in 1:nrow(learn_df)){
    indexs = which(synthetic_data_for_MISE[,1] < learn_df[i,2] &
                     synthetic_data_for_MISE[,1] >= learn_df[i,1] &
                     synthetic_data_for_MISE[,2] < learn_df[i,4] &
                     synthetic_data_for_MISE[,2] >= learn_df[i,3])
    synthetic_data_for_MISE$which_region_learn[indexs] = i
  }
  
  synthetic_data_on_boundary_or_outer = filter(synthetic_data_for_MISE, which_region_learn ==0)
  for(i in 1:nrow(learn_df)){
    indexs = which(synthetic_data_on_boundary_or_outer[,1] <= learn_df[i,2] &
                     synthetic_data_on_boundary_or_outer[,1] >= learn_df[i,1] &
                     synthetic_data_on_boundary_or_outer[,2] <= learn_df[i,4] &
                     synthetic_data_on_boundary_or_outer[,2] >= learn_df[i,3])
    synthetic_data_on_boundary_or_outer$which_region_learn[indexs] = i
  }
  
  synthetic_data_for_MISE = synthetic_data_for_MISE %>% as.data.frame()
  synthetic_data_for_MISE$which_region_learn[synthetic_data_for_MISE$which_region_learn == 0] = 
    synthetic_data_on_boundary_or_outer$which_region_learn
  
  
  
  if(exclude_uncovered){
    synthetic_data_for_MISE = filter(synthetic_data_for_MISE, which_region_learn != 0)  
    learn_den = empirical_density[synthetic_data_for_MISE$which_region_learn] 
    synthetic_data_for_MISE$learn_density = learn_den
    colnames(synthetic_data_for_MISE)[1:2]=c("x","y")
    synthetic_data_for_MISE = synthetic_data_for_MISE[synthetic_data_for_MISE$learn_density>0,]
  } else {
    learn_den = rep(0, nrow(synthetic_data_for_MISE))
    learn_den[synthetic_data_for_MISE$which_region_learn == 0] = 0 # uncovered
    
    covered_indices = (synthetic_data_for_MISE$which_region_learn != 0)
    learn_den[covered_indices] = 
      empirical_density[synthetic_data_for_MISE$which_region_learn[covered_indices]]
    
    synthetic_data_for_MISE$learn_density = learn_den
    colnames(synthetic_data_for_MISE)[1:2]=c("x","y")
  }
  
  return(synthetic_data_for_MISE)
}

get_emp_density = function(data, learn_df){
  n = nrow(data)
  emprical_density = rep(0, nrow(learn_df))
  for(i in 1:nrow(learn_df)){
    emprical_density[i] = sum(data[,1] <= learn_df[i,2] &
                                data[,1] > learn_df[i,1] &
                                data[,2] <= learn_df[i,4] &
                                data[,2] > learn_df[i,3]) / n / learn_df$area[i]
  }
  return(emprical_density)
}

# get_bins
get_bins = function(d, eps = 1, Kmax = 50){
  # if(ncol(d) > 2){
  #   d = cbind(d$X, d$Y)
  # }
  bins = search_multid(cbind(d$X,d$Y), eps, Kmax, dim_choose = "alternative")
  merged_res = multi_hist_merging(bins, eps)
  
  res_df = unlist(bins) %>% matrix(byrow=T, ncol = length(bins[[1]]))
  to_merge_learn = visual_2d_MDLhist(res_df, merged_res$g)
  b = produce_short_line_segs(res_df, to_merge_learn)
  visual = 
    ggplot() + 
    geom_segment(aes(x = b[1,],y=b[2,],xend = b[3,], yend=b[4,])) 
  print(visual)
  return(list(bins = bins, merged_res = merged_res, res_df = res_df))
}

# extract the time only as numbers, and min(d$Timestamp) = 0, unit = min
extract_mins = function(d){
  min_time = min(d$Timestamp)
  mins = difftime(d$Timestamp, min_time, units="mins") %>% as.numeric() %>% round(0)
  return(mins)
}

extract_hs = function(d){
  min_time = min(d$Timestamp)
  hs = difftime(d$Timestamp, min_time, units="hours") %>% as.numeric() %>% round(0)
  return(hs)
}