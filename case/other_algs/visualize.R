# this script collects functions that maps bins and data to contours / density tiles / data segmentation
get_partitions = function(res_df, merged_res, print=T, returnPlot=F){
  to_merge_learn = visual_2d_MDLhist(res_df, merged_res$g)
  b = produce_short_line_segs(res_df, to_merge_learn)
  visual = 
    ggplot() + 
    geom_segment(aes(x = b[1,],y=b[2,],xend = b[3,], yend=b[4,])) 
  if(print){
    print(visual)  
  }
  if(returnPlot){
    return(visual)
  }
}

get_density_tiles = function(res_df, d, empirical_density_per_region, eps, alpha=1, 
                             ggsave_path, ggsave_device = "png", 
                             returnFig=F, printFig=F, saveFig=T){
  learn_df = res_df %>% as.data.frame()
  learn_df[,"area"] = (learn_df[,2]-learn_df[,1])*(learn_df[,4]-learn_df[,3])
  colnames(learn_df) = c("left","right","down","up","area")
  
  synthetic_data_for_MISE = get_synthetic_data_for_MISE(d, learn_df, empirical_density_per_region, eps)
  
  d_frame = as.data.frame(d); colnames(d_frame) = c("x", "y")
  gg2 = ggplot(d_frame) + aes(x = X, y = y) +
    geom_tile(data = synthetic_data_for_MISE, aes(x = x, y = y,fill= learn_density, alpha=alpha)) +
    scale_fill_gradientn(colours=c("darkgreen", "green", "greenyellow", "yellow",
                                   "orange", "red", "darkred"), name = "density") +
    scale_alpha(range = c(0, 1), guide = "none")+
    theme(axis.title = element_blank(), text = element_text(size = 12)) 
  
  if(saveFig){
    ggsave(ggsave_path, gg2, ggsave_device)  
  }
  
  if(printFig){
    print(gg2)
  }
  
  if(returnFig){
    return(gg2)
  }
}

get_density_pixels = function(d, eps, res_df, empirical_density_per_region, exclude_uncovered=F){
  learn_df = res_df %>% as.data.frame()
  learn_df[,"area"] = (learn_df[,2]-learn_df[,1])*(learn_df[,4]-learn_df[,3])
  colnames(learn_df) = c("left","right","down","up","area")
  
  synthetic_data_for_MISE = 
    get_synthetic_data_for_MISE(d, learn_df, empirical_density_per_region, eps,
                                exclude_uncovered = exclude_uncovered)
  return(synthetic_data_for_MISE)
}

get_filled_contours = function(){
  
}