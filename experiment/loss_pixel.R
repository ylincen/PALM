loss_pixel = function(pixel_true, pixel_learn){
  # Input: 2 2-col matrix, representing the true and learn pixels.
  
  dist_based_true = rep(Inf, nrow(pixel_true))
  dist_based_learn = rep(Inf, nrow(pixel_learn))
  
  for(i in 1:nrow(pixel_true)){
    dist_based_true[i] = sweep(pixel_learn, 2, pixel_true[i,]) %>%
    {.^2} %>% apply(1,sum) %>% min()
  }
  
  for(i in 1:nrow(pixel_learn)){
    dist_based_learn[i] = sweep(pixel_true, 2, pixel_learn[i,]) %>%
    {.^2} %>% apply(1,sum) %>% min()
  }
  
  return(list(dist_based_true=dist_based_true, 
              dist_based_learn=dist_based_learn))
}