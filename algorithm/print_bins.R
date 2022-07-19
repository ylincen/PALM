require(ggplot2)
print_bins = function(bins){
  if(length(unique(sapply(bins, length))) != 1){
    stop("error in print_bins.R, bins must have the same length!")
  }
  num_dim = length(bins[[1]])/2
  
  bin_df = unlist(bins) %>% matrix(byrow = T, ncol = num_dim * 2) %>%
    data.frame()
  colnames(bin_df) = c("xmin","xmax","ymin","ymax")
  
  if(num_dim > 2){
    stop("error in print_bins.R, can only visualize 2D bins!")
  }
  
  p = ggplot(data = bin_df) + geom_rect(aes(xmin = xmin, xmax = xmax,
                                            ymin = ymin, ymax = ymax),
                                        fill = NA, colour = "black")
  print(p)
}