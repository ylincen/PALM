produce_short_line_segs = function(s, to_remove){
  # This function convert the cut_matrix into short line segments,
  #   to be used for visualizations
  
  #Input: 
  # s: split result (df or matrix)
  # m: removed short line segs due to merging
  
  #Output: 
  # line_seg_set = a matrix, each col is a line seg, the 5th row is either ...
  # ... 1 or 2, representing whether the line segment is vertical or horizontal
  
  s = as.matrix(s) # in case it is a df
  
  
  verticals = unique(s[,1:2] %>% as.numeric()) %>% sort()

  line_seg_set = matrix(nrow = 5)
  for(i in 1:length(verticals)){
    #cat("i:", i, "\n")
    relevant_s = s[apply(s[,1:2, drop = F], 1, function(y){verticals[i] %in% y}),]
    if(nrow(relevant_s) %>% is.null){
      relevant_s = matrix(relevant_s, byrow = T, ncol = 4)
    }
    shorts = relevant_s[,3:4] %>% as.numeric %>% 
      unique() %>% sort()
    
    shorts_mat = matrix(nrow = 5)[,NULL]
    for(j in 1:nrow(relevant_s)){
      #cat("i, j:", i, ",", j, "\n")
      # if(i==9 & j==5){
      #   browser()
      # }
      shorts_within = shorts[shorts >= relevant_s[j,3] &
                               shorts <= relevant_s[j,4]]
      if(length(shorts_within) <= 1){
        browser()
      }
      a = rbind(rep(verticals[i], (length(shorts_within)-1) ),
                shorts_within[1:(length(shorts_within)-1)],
                rep(verticals[i], (length(shorts_within)-1) ),
                shorts_within[-1],
                rep(1, (length(shorts_within)-1)))
      shorts_mat <- cbind(shorts_mat, a)
    }
    
    line_seg_set = cbind(line_seg_set, shorts_mat)
  }
  
  
  horizontals = unique(s[,3:4, drop = F] %>% as.numeric()) %>% sort()

  for(i in 1:length(horizontals)){
    relevant_s = s[apply(s[,3:4, drop = F], 1, function(y){horizontals[i] %in% y}),]
    
    relevant_s = matrix(relevant_s, ncol = 4)
    
    shorts = relevant_s[,1:2] %>% as.numeric() %>%
      unique() %>% sort()
    
    shorts_mat = matrix(nrow = 5)[,NULL]
    for(j in 1:nrow(relevant_s)){
      shorts_within = shorts[shorts >= relevant_s[j,1] &
                               shorts <= relevant_s[j,2]]
      if(length(shorts_within) <= 1){
        browser()
      }
      shorts_mat = shorts_mat %>% cbind(rbind(shorts_within[1:(length(shorts_within)-1)],
                                              rep(horizontals[i], (length(shorts_within)-1)),
                                              shorts_within[-1],
                                              rep(horizontals[i], (length(shorts_within)-1)),
                                              rep(2, (length(shorts_within)-1))))
    }
    
    line_seg_set = cbind(line_seg_set, shorts_mat)
  }
  
  line_seg_set = line_seg_set[,-1]
  line_seg_set = line_seg_set %>% t %>% unique %>% t
  
  #remove those boundaries that are already removed due to merging
  remove_col_index = rep(0,length(to_remove)/4)
  jj = 1
  
  to_remove = matrix(to_remove, ncol=4)
  
  if(length(to_remove) > 1){
    for(i in 1:ncol(line_seg_set)){
      yes_or_no = apply(to_remove, 1, function(y){
        all.equal(line_seg_set[1:4,i], y) %>% isTRUE()}) %>%
        any()
      if(yes_or_no){
        remove_col_index[jj] = i
        jj = jj + 1
      }
    }
    
    line_seg_set = line_seg_set[,-remove_col_index] # found a pitfall here!
  }
  
  
  
  #concatenate short lines connecting with each other
    index_to_concatenate = rep(0,ncol(line_seg_set) - 1)
    for(i in 1:(ncol(line_seg_set)-1)){
      if(line_seg_set[5,i] == 1){
        if(line_seg_set[1,i] == line_seg_set[1,i+1]){
          index_to_concatenate[i] = as.numeric(line_seg_set[4,i] == line_seg_set[2,i+1])
        }
      } else{
        if(line_seg_set[2,i] == line_seg_set[2,i+1]){
          index_to_concatenate[i] = as.numeric(line_seg_set[3,i] == line_seg_set[1,i+1])
        }
      }
    }
  
    line_seg_set_refine = matrix(rep(0,5 * (ncol(line_seg_set) -
                                              sum(index_to_concatenate))),
                                 nrow = 5)
  
  
    jj = 1
    line_seg_set_refine[,1] = line_seg_set[,1]
  
    for(i in 1:length(index_to_concatenate)){
      if(line_seg_set[5,i+1] == 1){
        if(index_to_concatenate[i] == 1){
          line_seg_set_refine[,jj] = c(line_seg_set_refine[1,jj],
                                      line_seg_set_refine[2,jj],
                                      line_seg_set_refine[3,jj],
                                      line_seg_set[4, i + 1],
                                      1)
        } else {
          jj = jj + 1
          line_seg_set_refine[,jj] = line_seg_set[,i+1]
        }
      } else{
        if(index_to_concatenate[i] == 1){
          line_seg_set_refine[,jj] = c(line_seg_set_refine[1,jj],
                                       line_seg_set_refine[2,jj],
                                       line_seg_set[3,i+1],
                                       line_seg_set_refine[4, jj],
                                       2)
        } else {
          jj = jj + 1
          line_seg_set_refine[,jj] = line_seg_set[,i+1]
        }
      }
    }
  
  
  
  
  return(line_seg_set_refine)
}