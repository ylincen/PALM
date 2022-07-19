visualize_twod_result = function(split_res, merge_res, eps, num_dim = 2){
  # This function visualizes the two-dimensional segmentation after 
  #   splitting and merging.
  
  
  cuts_mat = split_res[,1:(num_dim*2)] %>% as.matrix()
  cuts_mat = round(cuts_mat, -log10(eps))
  to_remove = visual_2d_MDLhist(cuts_mat, merge_result = merge_res$g)
  to_draw = produce_short_line_segs(cuts_mat, to_remove)
  to_draw = to_draw %>% t %>% as.data.frame()
  g2 = ggplot(data = to_draw) + geom_segment(aes(x = V1,
                                                 y = V2,
                                                 xend = V3,
                                                 yend = V4))
}