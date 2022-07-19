rm(list=ls())
# packages and code
require(dplyr)
require(gmp)
require(ggplot2)
require(MCMCpack)

# setwd("~/Dropbox/PALM-code-MLJ/")
source("load_sources.R")
source("./experiment/exp4/run-ipd.R")
source("get_bins.R")
source("./experiment/loss_pixel.R")
source("./experiment/shortline_to_pixel_points.R")
# generate the grid
seed_vec = sample.int(1e7,500)
exp3_fun = function(n, seed=NULL){
  
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  control_threshold = 0.1
  v = sample(c(runif(1,0 + control_threshold,0.5 - control_threshold),
               runif(1,0.5 + control_threshold, 1 - control_threshold)),
             1)
  
  eps = 0.001; Kmax = 10
  v = round(v, digits = -log10(eps))
  
  
  h = sample(c(runif(1,0 + control_threshold,0.5 - control_threshold),
               runif(1,0.5 + control_threshold, 1 - control_threshold)),
             1)
  
  h = round(h, digits = -log10(eps))
  
  
  # generate the data
  n = n
  x = c(runif(n/4,0,v), runif(n/4,0,v),
        runif(n/4,v,1), runif(n/4,v,1))
  y = c(runif(n/4,0,h), runif(n/4,h,1),
        runif(n/4,0,h), runif(n/4,h,1))
  plot(x,y)
  abline(h=h,v=v,col="red")
  d = cbind(x,y)
  #learn the boundary
  time1 = proc.time()
  
  res = trainPALM(d, eps, Kmax)
  time2 = proc.time()
  
  merge_result = res$merged_res$g
  
  to_merge_learn = visual_2d_MDLhist(res, merge_result)
  b = produce_short_line_segs(res$res_df, to_merge_learn)
  
  sls = b %>% t() %>% .[,1:4] %>%
    as.data.frame()
  
  ggplot(data = sls) + geom_segment(aes(x = V1,
                                        y = V2,
                                        xend = V3,
                                        yend = V4), size = 0.5, col = "black",
                                    lty = 1)
  
  # ipd algorithm
  ipd_res = run_ipd(d, eps, Kmax)
  cuts1 = ipd_res[[1]]; cuts2 = ipd_res[[2]]
  b_ipd = c(0,cuts1,1) %>% rbind(0) %>% rbind(c(0,cuts1,1)) %>% 
    rbind(1) %>% rbind(1)
  b_ipd = 0 %>% rbind(c(0,cuts2,1)) %>% rbind(1) %>%
    rbind(c(0,cuts2,1)) %>% rbind(2) %>% {cbind(b_ipd, .)}

  
  a = c(0,v,1) %>% rbind(0) %>% rbind(c(0,v,1)) %>% rbind(1) %>% rbind(1)
  a = cbind(a, 0 %>% rbind(c(0,h,1)) %>% rbind(1) %>%
              rbind(c(0,h,1)) %>% rbind(2)) 
  
  pixels_true = shortline_to_pixel_points(a, 0.01)
  pixels_snm = shortline_to_pixel_points(b, 0.01)
  pixels_ipd = shortline_to_pixel_points(b_ipd, 0.01)
  
  loss_pixels_snm = loss_pixel(pixel_true = pixels_true,pixel_learn = pixels_snm)
  loss_pixels_ipd = loss_pixel(pixel_true = pixels_true,pixel_learn = pixels_ipd)
  
  loss_pixels_ipd$dist_based_true %>% sum()
  loss_pixels_ipd$dist_based_learn %>% sum()
  
  loss_pixels_snm$dist_based_learn %>% sum()
  loss_pixels_snm$dist_based_true %>% sum()
  
  
  #cat("seed is", seed)
  return(list(seed = seed, loss_pixels_snm = loss_pixels_snm,
              loss_pixels_ipd = loss_pixels_ipd,
              num_short_line_ipd = ncol(b_ipd),
              num_short_line_snm = ncol(b),
              cuts = list(cuts1, cuts2),
              real_cuts = c(h,v),
              cuts_snm = b))
}



n_vec = c(seq(100,1000,by=100), seq(3000,9000,by=2000))
for(jjjjjjjj in 1:length(n_vec)){
  Final_res = list()
  for(iiii in 1:100){
    cat("working on ",iiii,"with n equal to ", n_vec[jjjjjjjj],"\n")
    n = n_vec[jjjjjjjj]
    seed= seed_vec[iiii]
  
    set.seed(seed)
    Final_res[[iiii]] = exp3_fun(n, seed)
  }
  file_name = paste("./experiment/exp4/res_",n,".Rdata",sep="")
  save(Final_res,file = file_name)
}

