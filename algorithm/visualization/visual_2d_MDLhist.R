visual_2d_MDLhist = function(split_result, merge_result, 
                             show_points = F, points = NULL){
  
  # This function creates a matrix representing the "line segments"
  #   that should be removed due to the merging, just for visualizations.
  to_merge = matrix(ncol = 4)
  
  if(is.list(split_result)){
    d = split_result$df %>% unname()
  } else {
    d = split_result
  }
  
  
  
  
  
  groups = components(merge_result)$membership %>% 
    table() %>% .[.>1] %>% 
    names() %>% as.numeric()
  
  if(length(groups)>=1){
    for(i in 1:length(groups)){
      candidates_to_remove = which(components(merge_result)$membership == groups[i])
      candidates_pairs = combn(candidates_to_remove, 2)
      for(j in 1:ncol(candidates_pairs)){
        #if(j==4) browser()
        indexs = candidates_pairs[,j]
        common_edge = overlap(d[indexs[1],1:4], d[indexs[2],1:4], 
                              show_common_edge = T) 
        common_edge = common_edge %>% unlist()
        if(length(common_edge) == 4){
          to_merge = rbind(to_merge, common_edge)
        }
      }
    }
  }
  
  
  to_merge = unname(to_merge)
  to_merge = to_merge[-1,]
  
  return(to_merge)
}