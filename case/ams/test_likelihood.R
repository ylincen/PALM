# get the likelihood on the test data; 
# this function is also used by the other two case study datasets.
get_likelihood = function(res, test_index){
  res_df = res$res_df
  empirical_density_per_region = res$empirical_density_per_region
  learn_df = res_df %>% as.data.frame()
  learn_df[,"area"] = (learn_df[,2]-learn_df[,1])*(learn_df[,4]-learn_df[,3])
  colnames(learn_df) = c("left","right","down","up","area")
  
  test_data_for_MISE = get_test_data_for_MISE(d[test_index,], learn_df, 
                                              empirical_density_per_region, 
                                              eps=0.001, exclude_uncovered = F)
  return(test_data_for_MISE)
}