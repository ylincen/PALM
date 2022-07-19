# This script collects functions that can make data to bins;

# Input: d: a matrix, # of cols representing the dims. 

# train PALM 
trainPALM = function(d, eps, Kmax, save_res = F, save_path = NULL, start_dim = 1){
  bins = search_multid(d, eps, Kmax, dim_choose = "alternative", start_dim = start_dim)
  merged_res = multi_hist_merging(bins, eps=eps)
  res_df = unlist(bins) %>% matrix(byrow=T, ncol = length(bins[[1]]))
  empirical_density_per_region = merged_res$counts / nrow(d) / merged_res$vols
  
  if(save_res){
    save(list=c("bins", "merged_res", "res_df", "empirical_density_per_region"),
         file = save_path)
  }
  
  return(list(bins=bins, merged_res=merged_res,
              res_df = res_df, empirical_density_per_region=empirical_density_per_region))
}


# train PALM sep
trainPALMsep = function(d, eps, Kmax, force_not_to_merge=T, save_res = F, save_path = NULL){
  bins_x = search_multid(d[,1] %>% t %>% t, eps, Kmax, dim_choose = "alternative")
  bins_y = search_multid(d[,2] %>% t %>% t, eps, Kmax, dim_choose = "alternative")
  
  cut_x = bins_x %>% unlist %>% unique %>% sort()
  cut_y = bins_y %>% unlist %>% unique %>% sort()
  bins = list(); l = 1
  for(i in 1:length(bins_x)){
    for(j in 1:length(bins_y)){
      bins[[l]] = c(bins_x[[i]], bins_y[[j]])
      attributes(bins[[l]]) = attributes(bins_x[[i]])
      attr(bins[[l]], "boundary") = cbind(attr(bins_x[[i]], "boundary"), 
                                          attr(bins_y[[j]], "boundary"))
      attr(bins[[l]], "index") = intersect(attr(bins_x[[i]], "index"), 
                                           attr(bins_y[[j]], "index"))
      l = l + 1
    }
  }
  merged_res = multi_hist_merging(bins, eps=eps, force_not_to_merge = force_not_to_merge)
  res_df = unlist(bins) %>% matrix(byrow=T, ncol = length(bins[[1]]))
  to_merge_learn = visual_2d_MDLhist(res_df, merged_res$g)
  
  empirical_density_per_region = merged_res$counts / nrow(d) / merged_res$vols
  
  if(save_res){
    save(list=c("bins", "merged_res", "res_df", "empirical_density_per_region"),
         file = save_path)
  }
  
  return(list(bins=bins, merged_res=merged_res,
              res_df = res_df, empirical_density_per_region=empirical_density_per_region))
}

# train fixed-size hist
train_fixedsize_bins = function(d, eps, Kmax, force_not_to_merge=T, save_res = F, 
                                save_path = NULL, breaks = "Sturges"){
  histres = hist(d[,1], plot=F, breaks=breaks)
  histres2 = hist(d[,2], plot=F, breaks=breaks)
  
  bins_x = list(); bins_y = list()
  which_bins_x = findInterval(d[,1], histres$breaks, rightmost.closed = T)
  which_bins_y = findInterval(d[,2], histres2$breaks, rightmost.closed = T)
  
  for(i in 1:(length(histres$breaks)-1)){
    bins_x[[i]] = histres$breaks[i:(i+1)]
    attr(bins_x[[i]], "index") = which(which_bins_x == i)
    attr(bins_x[[i]], "boundary") = t(bins_x[[i]]) %>% t
  }
  
  for(i in 1:(length(histres2$breaks)-1)){
    bins_y[[i]] = histres2$breaks[i:(i+1)]
    attr(bins_y[[i]], "index") = which(which_bins_y == i)
    attr(bins_y[[i]], "boundary") = t(bins_y[[i]]) %>% t
  }
  
  cut_x = bins_x %>% unlist %>% unique %>% sort()
  cut_y = bins_y %>% unlist %>% unique %>% sort()
  bins = list(); l = 1
  for(i in 1:length(bins_x)){
    for(j in 1:length(bins_y)){
      bins[[l]] = c(bins_x[[i]], bins_y[[j]])
      attributes(bins[[l]]) = attributes(bins_x[[i]])
      attr(bins[[l]], "boundary") = cbind(attr(bins_x[[i]], "boundary"), 
                                          attr(bins_y[[j]], "boundary"))
      attr(bins[[l]], "index") = intersect(attr(bins_x[[i]], "index"), 
                                           attr(bins_y[[j]], "index"))
      l = l + 1
    }
  }
  merged_res = multi_hist_merging(bins, eps=eps, force_not_to_merge = force_not_to_merge)
  res_df = unlist(bins) %>% matrix(byrow=T, ncol = length(bins[[1]]))
  to_merge_learn = visual_2d_MDLhist(res_df, merged_res$g)
  
  empirical_density_per_region = merged_res$counts / nrow(d) / merged_res$vols
  
  if(save_res){
    save(list=c("bins", "merged_res", "res_df", "empirical_density_per_region"),
         file = save_path)
  }
  
  return(list(bins=bins, merged_res=merged_res,
              res_df = res_df, empirical_density_per_region=empirical_density_per_region))
}

# train IPD
trainIPD = function(d, eps, Kmax, force_not_to_merge=T, save_res = F, save_path = NULL,
                    tmp_file_path = "./", tmp_outfile_path = "./"){
  dipd = cbind(d, rep(0, nrow(d)))
  #parameters:
  FILE_INPUT = paste(tmp_file_path, "ipd_data.csv", sep="")    # name of input file
  write.table(dipd, file = FILE_INPUT, sep=";", row.names = F, col.names = F)
  
  FILE_CP_OUTPUT = paste(tmp_outfile_path, "ipdcuts.txt", sep="")         # name of output file for cut points per dimension
  FILE_RUNTIME_OUTPUT = paste(tmp_outfile_path, "ipdruntime.txt", sep="")    # name of output file for runtime in microseconds
  FILE_DATA_OUTPUT = paste(tmp_outfile_path, "ipdoutput.txt", sep="")    # name of output file for discretized data in .arff format
  NUM_ROWS = nrow(d)             # number of data points
  NUM_MEASURE_COLS = 2      # number of numeric dimensions
  NUM_CAT_CONTEXT_COLS = 0  # number of categorical dimensions
  MAX_VAL = max(d)              # maximum value, used for normalization
  METHOD = 0
  
  command_line = paste("java -jar ~/Documents/PALM_R/sdm21-dev/experiment/exp4/ipd/ipd.jar -FILE_INPUT",
                       FILE_INPUT,
                       "-FILE_CP_OUTPUT",
                       FILE_CP_OUTPUT,
                       "-FILE_RUNTIME_OUTPUT",
                       FILE_RUNTIME_OUTPUT,
                       "-FILE_DATA_OUTPUT",
                       FILE_DATA_OUTPUT,
                       "-NUM_ROWS",
                       NUM_ROWS,
                       "-NUM_MEASURE_COLS",
                       NUM_MEASURE_COLS,
                       "-NUM_CAT_CONTEXT_COLS",
                       NUM_CAT_CONTEXT_COLS,
                       "-MAX_VAL",
                       MAX_VAL,
                       "-METHOD",
                       METHOD,
                       sep = " ")
  
  system(command_line)
  
  cuts = read.table(FILE_CP_OUTPUT, 
                    sep = "\n", colClasses = "character")
  
  seprate_index = which(cuts$V1 == "-------------------------------------")[1]
  cuts1 = cuts$V1[1:seprate_index] %>% as.numeric() %>% .[!is.na(.)]
  cuts1 = c( min(d[,1]), cuts1)
  
  cuts2 = cuts$V1[-(1:seprate_index)] %>% as.numeric() %>% .[!is.na(.)]
  cuts2 = c(min(d[,2]), cuts2)
  
  bins_x = list(); bins_y = list()
  which_bins_x = findInterval(d[,1], cuts1, rightmost.closed = T)
  which_bins_y = findInterval(d[,2], cuts2, rightmost.closed = T)
  
  for(i in 1:(length(cuts1)-1)){
    bins_x[[i]] = cuts1[i:(i+1)]
    attr(bins_x[[i]], "index") = which(which_bins_x == i)
    attr(bins_x[[i]], "boundary") = t(bins_x[[i]]) %>% t
  }
  
  for(i in 1:(length(cuts2)-1)){
    bins_y[[i]] = cuts2[i:(i+1)]
    attr(bins_y[[i]], "index") = which(which_bins_y == i)
    attr(bins_y[[i]], "boundary") = t(bins_y[[i]]) %>% t
  }
  
  cut_x = bins_x %>% unlist %>% unique %>% sort() 
  cut_y = bins_y %>% unlist %>% unique %>% sort() 
  bins = list(); l = 1
  for(i in 1:length(bins_x)){
    for(j in 1:length(bins_y)){
      bins[[l]] = c(bins_x[[i]], bins_y[[j]])
      attributes(bins[[l]]) = attributes(bins_x[[i]])
      attr(bins[[l]], "boundary") = cbind(attr(bins_x[[i]], "boundary"), 
                                          attr(bins_y[[j]], "boundary"))
      attr(bins[[l]], "index") = intersect(attr(bins_x[[i]], "index"), 
                                           attr(bins_y[[j]], "index"))
      l = l + 1
    }
  }
  merged_res = multi_hist_merging(bins, eps=eps, force_not_to_merge = force_not_to_merge)
  res_df = unlist(bins) %>% matrix(byrow=T, ncol = length(bins[[1]]))
  to_merge_learn = visual_2d_MDLhist(res_df, merged_res$g)
  
  empirical_density_per_region = merged_res$counts / nrow(d) / merged_res$vols
  
  if(save_res){
    save(list=c("bins", "merged_res", "res_df", "empirical_density_per_region"),
         file = save_path)
  }
  
  return(list(bins=bins, merged_res=merged_res,
              res_df = res_df, empirical_density_per_region=empirical_density_per_region))
}


