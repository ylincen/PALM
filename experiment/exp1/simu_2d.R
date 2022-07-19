simu_2d = function(K, merge_rate, outer_bound, eps, 
                   num_cut, minimum_cut_distance){
  
  d = data.frame(as.list(outer_bound))
  
  # prepare for the visualizaiton of map segmentation
  to_draw = matrix(ncol = 4)
  to_merge = matrix(ncol = 4)
  

  ## Do the split
  for(k in 1:K){
    col_index = c(k %% 2 * (-2) + 3, k %% 2 * (-2) + 4)
    d1 = data.frame()
    for(i in 1:nrow(d)){
      row = d[i,] %>% unlist()
      num_cut = num_cut
      
      if( (row[col_index[2]] - row[col_index[1]]) < 3 * minimum_cut_distance ){
        # the condition is to avoid possible computation error 
        cut = c(row[col_index[1]], row[col_index[2]])
        
      } else {
        
        if(seq(row[col_index[1]] + minimum_cut_distance, 
               row[col_index[2]] - minimum_cut_distance, 
               by = minimum_cut_distance) %>% length() == 1){
          cut = c(row[col_index[1]], 
                  seq(row[col_index[1]] + minimum_cut_distance, 
                             row[col_index[2]] - minimum_cut_distance, 
                             by = minimum_cut_distance),
                  row[col_index[2]]) 
          
        } else{
          cut = c(row[col_index[1]], 
                  sample(seq(row[col_index[1]] + minimum_cut_distance, 
                             row[col_index[2]] - minimum_cut_distance, 
                             by = minimum_cut_distance), 
                         size = min(num_cut, 
                                    round((row[col_index[2]] - 
                                       row[col_index[1]])/minimum_cut_distance - 1)), 
                         replace = F),
                  row[col_index[2]]) %>% sort()
        }

        # prepare for the map segmentation visualization
        to_draw_this_step = cut[2:(length(cut)-1)]
        
        if(k %% 2 == 1){
          to_draw_this_step = rbind(to_draw_this_step, row[col_index[1]+2],
                                    to_draw_this_step, row[col_index[2]+2]) %>% 
            t() %>% unname()
        } else {
          to_draw_this_step = rbind(row[col_index[1]-2], to_draw_this_step,
                                    row[col_index[2]-2], to_draw_this_step) %>% 
            t() %>%
            unname()
        }
        
        to_draw = rbind(to_draw %>% unname, to_draw_this_step)
      }
      
      
      cut_mat = t(rbind(cut[1:(length(cut)-1)], cut[-1]))
      dd = matrix(rep(row, length(cut)-1), ncol = 4, byrow = T)
      dd[,col_index] = cut_mat
      
      d1 = rbind(d1,dd)
    }
    d = d1
  }
  
  ## Get the neighbour list of each cell
  neighbor_list = list()
  for(i in 1:(nrow(d)-1)){
    for(j in (i+1):nrow(d)){
      if(overlap(unlist(d[i,]), unlist(d[j,]))){
        neighbor_list[i] = list(c(unlist(neighbor_list[i]),j))
      }
    }
  }
  
  ## Merge some of the areas
  neigh_2darray = sapply(neighbor_list, length) %>% 
  {rep(1:length(neighbor_list),.)} %>%
    rbind(unlist(neighbor_list))
  
  
  merge_or_not = rep(0, ncol(neigh_2darray))
  
  #force at least two pairs of neighbors to merge
  while(all(merge_or_not==0)){
    merge_or_not = sample(c(1,0), size = ncol(neigh_2darray),
                          prob = c(merge_rate, 1-merge_rate),
                          replace = T) %>% as.logical
  }
  
  merge_2darray = neigh_2darray[,merge_or_not]
  
  require(igraph)
  g = graph(edges = as.vector(merge_2darray), n = nrow(d), directed = F)
  
  
  ## Remove common edges when two areas are merged
  groups = components(g)$membership %>% 
    table() %>% .[.>1] %>% 
    names() %>% as.numeric()
  
  for(i in 1:length(groups)){
    candidates_to_remove = which(components(g)$membership == groups[i])
    candidates_pairs = combn(candidates_to_remove, 2)
    for(j in 1:ncol(candidates_pairs)){
      indexs = candidates_pairs[,j]
      common_edge = overlap(d[indexs[1],1:4], d[indexs[2],1:4], 
                            show_common_edge = T) %>% unlist()
      
      if(length(common_edge) == 4){
        to_merge = rbind(to_merge, common_edge)
      }
    }
  }
  
  to_merge = unname(to_merge)
  to_merge = to_merge[-1,]
  
  to_draw = to_draw[-1,]
  
  
  ## Calculate the arae of each cell, for later use
  d$area = (d$V2 - d$V1) * (d$V4 - d$V3)
  colnames(d)[1:4] = c('left','right','down','up')
  
  
  
  return(list(d = d,
              merge_list = components(g),
              neighbor_list = neighbor_list,
              to_draw = to_draw, to_merge = to_merge))
}
