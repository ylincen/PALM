# This scripts is used for "revealing the ground truth partition", i.e., 
# the first experiment in the paper. 

rm(list = ls())

#setwd("~/Dropbox/PALM-code-MLJ/")
source("load_sources.R")
source("get_bins.R")
# packages and code
require(dplyr)
require(gmp)

setwd("./experiment/exp1")
source("simu_2d.R")
source('simu_2d_data.R')
source("../loss_pixel.R")
source("../shortline_to_pixel_points.R")


#Output
n_vec = c(5e3,7e3,1e4,3e4,5e4,7e4,1e5,3e5,5e5,7e5,1e6, 3e6, 5e6, 7e6)
for(n in n_vec){
  seed_vec = rep(0,500)
  loss_based_true = rep(0,500)
  loss_based_learn = rep(0,500)
  MISE = rep(0,500)
  MISE_naive = rep(0,500)
  num_true_cells = rep(0,500)
  
  time1 = proc.time()
  
  for(iterations in 1:500){
    cat("working on iteration ",iterations,"and n is ", n, "\n")
    
    K = 2
    merge_rate = 0.4
    outer_bound = c(0,1,0,1)
    eps = 0.001
    num_cut = 5 #num of cut will be fixed anyway, see simu_2d.R
    minimum_cut_distance = 5 * eps
    Kmax = 10
    
    seed = sample.int(1e7,1)
    seed_vec[iterations] = seed
    set.seed(seed)
    
    # data generation
    boundaries = simu_2d(K = K, merge_rate = merge_rate, 
                         outer_bound = outer_bound, eps = eps, 
                         num_cut = num_cut,
                         minimum_cut_distance = minimum_cut_distance)
    
    data = simu_2d_data(d = boundaries$d, n = n, 
                        memberships = boundaries$merge_list$membership,
                        eps = eps,neighborlist = boundaries$neighbor_list,
                        check_density = F)
    
    # true boundaries
    a = produce_short_line_segs(boundaries$d[,1:4], to_remove = boundaries$to_merge)
    
    d = data$data[,1:2]
    
    time1 = proc.time()
  
    res = trainPALM(d, eps, Kmax)
    time2 = proc.time()
    
    merge_result = res$merged_res$g
    
    to_merge_learn = visual_2d_MDLhist(res, merge_result)
    b = produce_short_line_segs(res$res_df, to_merge_learn)
    
    #short lines to pixels
    pixels_true = shortline_to_pixel_points(a, eps*10)
    pixels_learn = shortline_to_pixel_points(b, eps*10)
    
    # pixel loss
    loss_pixels = loss_pixel(pixel_true = pixels_true,pixel_learn = pixels_learn)
    loss_based_true[iterations] = loss_pixels$dist_based_true %>% sum()
    loss_based_learn[iterations] = loss_pixels$dist_based_learn %>% sum()
    
    print(ggplot() + geom_segment(aes(x = a[1,],y=a[2,],xend = a[3,], yend=a[4,])) +
            geom_segment(aes(x = b[1,],y=b[2,],xend = b[3,], yend=b[4,]),color="red",
                         lty = 6))
    
    # density loss
    learn_df = data.frame(res$res_df %>% unname)
    learn_df[,"area"] = (learn_df[,2]-learn_df[,1])*(learn_df[,4]-learn_df[,3])
    colnames(learn_df) = c("left","right","down","up","area")
    
    emprical_density = rep(0, nrow(learn_df))
    for(i in 1:nrow(learn_df)){
      emprical_density[i] = sum(data$data[,1] <= learn_df[i,2] &
                                  data$data[,1] > learn_df[i,1] &
                                  data$data[,2] <= learn_df[i,4] &
                                  data$data[,2] > learn_df[i,3]) / n / learn_df$area[i]
    }
    
    synthetic_data_for_MISE = expand.grid(seq(0,1,by=eps),seq(0,1,by=eps))
    
    synthetic_data_for_MISE["which_region_learn"] = rep(0,nrow(synthetic_data_for_MISE))
    synthetic_data_for_MISE["which_region_true"] = rep(0,nrow(synthetic_data_for_MISE))
    for(i in 1:nrow(learn_df)){
      indexs = which(synthetic_data_for_MISE[,1] <= learn_df[i,2] &
                       synthetic_data_for_MISE[,1] > learn_df[i,1] &
                       synthetic_data_for_MISE[,2] <= learn_df[i,4] &
                       synthetic_data_for_MISE[,2] > learn_df[i,3])
      synthetic_data_for_MISE$which_region_learn[indexs] = i
    }
    for(i in 1:nrow(boundaries$d)){
      indexs = which(synthetic_data_for_MISE[,1] <= boundaries$d[i,2] &
                       synthetic_data_for_MISE[,1] > boundaries$d[i,1] &
                       synthetic_data_for_MISE[,2] <= boundaries$d[i,4] &
                       synthetic_data_for_MISE[,2] > boundaries$d[i,3])
      synthetic_data_for_MISE$which_region_true[indexs] = i
    }
    
    synthetic_data_for_MISE = synthetic_data_for_MISE[synthetic_data_for_MISE$which_region_learn!=0 & synthetic_data_for_MISE$which_region_true!=0,]
    MISE[iterations] = sum((emprical_density[synthetic_data_for_MISE$which_region_learn] - 
                              data$df$theoretical_density[synthetic_data_for_MISE$which_region_true])^2
    )*(1/nrow(synthetic_data_for_MISE))
    #MISE
    MISE_naive[iterations] = sum((1 - data$df$theoretical_density[synthetic_data_for_MISE$which_region_true])^2)*
      (1/nrow(synthetic_data_for_MISE))
    
    num_true_cells[iterations] = boundaries$merge_list$no
  }
  
  save(list = c("seed_vec",
                "loss_based_true",
                "loss_based_learn" ,
                "MISE",
                "MISE_naive" ,
                "num_true_cells"),
       file = paste("./exp1_res_",n,".Rdata",sep=""))
  
  # proc.time() - time1
}
