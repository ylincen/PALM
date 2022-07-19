simu_2d_data = function(d, n, memberships, eps, 
                        density_diff_ratio_threshold = 0.05,
                        check_density = T,
                        neighborlist){
  # This function generates data from the map-segmentation ...
  # ... given by simu_2d
  
  ## simulate the density
  num_groups = max(memberships)
  
  density = runif(num_groups, 0, 10)
  density_each_row = rep(0, nrow(d))
  
  for(i in 1:nrow(d)){
    density_each_row[i] = density[memberships[i]]
  }

  check_success = F
  
  if(check_density){
    while(!check_success){
      density = runif(num_groups, 0, 10)
      if(min(density) < 0.2){
        print("density too low, next one")
        next()
      }
      density_each_row = rep(0, nrow(d))
      
      for(i in 1:nrow(d)){
        density_each_row[i] = density[memberships[i]]
      }
      
      check_success = T
      
      for(i in 1:(nrow(d)-1)){
        members = which(memberships == memberships[i])
        to_compare = neighborlist[[i]][!(neighborlist[[i]] %in% members)]
        
        density_diff_ratio = (density_each_row[i] - density_each_row[to_compare]) %>%
          abs %>% {./density_each_row[i]}
        if(any(density_diff_ratio < density_diff_ratio_threshold)){
          check_success = F
          break()
        } 
      }
    }
  }
  
  theta = density_each_row * d$area
  d$theta = theta/sum(theta) #multinomial coeffiencts should be a simplex
  
  which_region = sample.int(nrow(d), prob = d$theta, replace = T, size = n)
  
  x = y = rep(0,n)
  for(i in 1:nrow(d)){
    x[which_region == i] = runif(sum(which_region == i), 
                                 min = d[i,1], max = d[i,2])
    #Note: eps should be the same as simu_2d function
    y[which_region == i] = runif(sum(which_region == i), 
                                 min = d[i,3], max = d[i,4])
  }
  
  theta_hat = table(which_region) %>% .[order(names(.) %>% as.numeric)]
  
  # in case some regions have 0 data points
  if(length(theta_hat) != nrow(d)){
    theta_hat[(c(1:nrow(d)) %>% 
                 as.character())[!(c(1:nrow(d)) %>% 
                                     as.character() %in% 
                                     (theta_hat %>% names()))]] = 0
  }
  
  d$theta_hat = theta_hat/n
  
  
  data = data.frame(x,y,which_region, 
                    memberships = memberships[which_region])
  
  #calculate some value for later use
  d$theoretical_density = d$theta/d$area
  
  return(list(data = data, df = d))
}
