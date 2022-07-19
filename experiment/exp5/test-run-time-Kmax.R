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

Ks = 10:30
num_iter = 500
set.seed(1)
ts = rep(0, length(Ks))
n = 500
for(i in 1:length(Ks)){
  K = Ks[i]
  for(j in 1:num_iter){
    t0 = proc.time()[3]
    data = cbind(rnorm(n, 0, 1),
                 rnorm(n, 0, 1))
    bins = search_multid(data, eps = 0.01, Kmax = K, dim_choose = "alternative")
    merge_res = multi_hist_merging(bins, eps = 0.01)
    t1 = proc.time()[3] - t0
    ts[i] = ts[i] + t1
  }
  ts[i] = ts[i] / num_iter
  cat("n: ", n, "K: ", K, "time: ", ts[i])
  save(ts, file = "./experiment/exp5/ts_wrtKmax_new.Rdata")
}

