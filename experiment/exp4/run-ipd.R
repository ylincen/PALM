
run_ipd = function(d, eps, Kmax){
  dipd = cbind(d, rep(0, nrow(d)))
  system("mkdir -p ./experiment/exp4/ipd_tmp") # create a tmp folder if it doesn't exist
  write.table(dipd, file = "./experiment/exp4/ipd_tmp/ipd_data.csv", sep=";", row.names = F, col.names = F)
  #parameters:
  FILE_INPUT = "./experiment/exp4/ipd_tmp/ipd_data.csv"    # name of input file
  FILE_CP_OUTPUT = "./experiment/exp4/ipd_tmp/ipdcuts.txt"        # name of output file for cut points per dimension
  FILE_RUNTIME_OUTPUT = "./experiment/exp4/ipd_tmp/ipdruntime.txt"   # name of output file for runtime in microseconds
  FILE_DATA_OUTPUT = "./experiment/exp4/ipd_tmp/ipdoutput.txt"   # name of output file for discretized data in .arff format
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
  
  FILE_CP_OUTPUT = "./experiment/exp4/ipd_tmp/ipdcuts.txt" 
  cuts = read.table(FILE_CP_OUTPUT, 
                    sep = "\n", colClasses = "character")
  
  seprate_index = which(cuts$V1 == "-------------------------------------")[1]
  cuts1 = cuts$V1[1:seprate_index] %>% as.numeric() %>% .[!is.na(.)]
  #cuts1 = cuts1[1:(length(cuts1)-1)]
  cuts1 = c( min(d[,1]), cuts1)
  
  cuts2 = cuts$V1[-(1:seprate_index)] %>% as.numeric() %>% .[!is.na(.)]
  #cuts2 = cuts2[1:(length(cuts2)-1)]
  cuts2 = c(min(d[,2]), cuts2)
  
  return(list(cuts1, cuts2))
}

