
run_ipd = function(d, eps, Kmax){
  dipd = cbind(d, rep(0, nrow(d)))
  system("mkdir -p ipd_tmp") # create a tmp folder if it doesn't exist
  write.table(dipd, file = "./ipd_tmp/ipd_data.csv", sep=";", row.names = F, col.names = F)
  #parameters:
  FILE_INPUT = "./ipd_tmp/ipd_data.csv"    # name of input file
  FILE_CP_OUTPUT = "./ipd_tmp/ipdcuts.txt"        # name of output file for cut points per dimension
  FILE_RUNTIME_OUTPUT = "./ipd_tmp/ipdruntime.txt"   # name of output file for runtime in microseconds
  FILE_DATA_OUTPUT = "./ipd_tmp/ipdoutput.txt"   # name of output file for discretized data in .arff format
  NUM_ROWS = nrow(d)             # number of data points
  NUM_MEASURE_COLS = 2      # number of numeric dimensions
  NUM_CAT_CONTEXT_COLS = 0  # number of categorical dimensions
  MAX_VAL = max(d)              # maximum value, used for normalization
  METHOD = 0
  
  
  command_line = paste("java -jar ./experiment/exp4/ipd/ipd.jar -FILE_INPUT",
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
  
  FILE_CP_OUTPUT = "./ipd_tmp/ipdcuts.txt" 
  cuts = read.table(FILE_CP_OUTPUT, 
                    sep = "\n", colClasses = "character")
  
  seprate_index = which(cuts$V1 == "-------------------------------------")[1]
  cuts1 = cuts$V1[1:seprate_index] %>% as.numeric() %>% .[!is.na(.)]
  #cuts1 = cuts1[1:(length(cuts1)-1)]
  cuts1 = c( min(d[,1]), cuts1)
  
  cuts2 = cuts$V1[-(1:seprate_index)] %>% as.numeric() %>% .[!is.na(.)]
  #cuts2 = cuts2[1:(length(cuts2)-1)]
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
  merged_res = multi_hist_merging(bins, eps=eps, force_not_to_merge = T)
  res_df = unlist(bins) %>% matrix(byrow=T, ncol = length(bins[[1]]))
  to_merge_learn = visual_2d_MDLhist(res_df, merged_res$g)
  
  empirical_density_per_region = merged_res$counts / nrow(d) / merged_res$vols
  
  a = get_density_tiles(res_df, d, empirical_density_per_region, eps, alpha=1, 
                        ggsave_path = "./ipd_tmp/ipd_tile.png", ggsave_device = "png", 
                        returnFig=T, printFig=T, saveFig=T)
  save(list=c("bins", "merged_res", "res_df", "empirical_density_per_region"),
       file = "./ipd_tmp/ipd.Rdata")
}

