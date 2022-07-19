rm(list = ls())
#setwd("~/Dropbox/PALM-code-MLJ/")

# packages and code
require(dplyr)
require(gmp)

source("./load_sources.R")
source("./experiment/exp1/simu_2d.R")
source('./experiment/exp1/simu_2d_data.R')
source("./experiment/loss_pixel.R")
source("./experiment/shortline_to_pixel_points.R")

num_iter = 500
set.seed(1)
eps = 0.01; Kmax = 20
n = 10000; n_use = 1000
Es = seq(100,1000,100)
ts = rep(0, length(Es))

for(i in 1:length(Es)){
  E = Es[i]
  data_range = E * eps
  for(j in 1:num_iter){
    t0 = proc.time()[3]
    data = cbind(rnorm(n, 0, data_range/2),
                 rnorm(n, 0, data_range/2))
    data = data[data[,1] < data_range/2 & 
                  data[,1] >= -data_range/2 &
                  data[,2] < data_range/2 & 
                  data[,2] >= -data_range/2,]
    data = data[sample(nrow(data), size = n_use, replace = F), ]
    
    bins = search_multid(data, eps = 0.01, Kmax = 20, dim_choose = "alternative")
    merge_res = multi_hist_merging(bins, eps = 0.01)
    t1 = proc.time()[3] - t0
    ts[i] = ts[i] + t1
  }
  ts[i] = ts[i] / num_iter
  cat("n: ", n, "E: ", E, "time: ", ts[i])
  save(ts, file = "./experiment/exp5/ts_wrt_E.Rdata")
}

