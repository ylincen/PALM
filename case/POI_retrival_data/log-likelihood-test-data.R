rm(list = ls())
#setwd("~/Dropbox/PALM-code-MLJ/")


source('load_sources.R')
source('./case/DinoFun/mini1/explore-utils.R')
source("./get_bins.R")
source("./case/ams/test_likelihood.R")
require(dplyr)
require(ks)
require(ggplot2)
require(MASS)
require(data.table)
d = fread("./case/POI_retrieval_data/chengdu_train.txt")

dd = d[, c(1,3,4,7,8,9,10)]
colnames(dd) = c("ID", "s_long", "s_lat", "search_long", "search_lat",
                 "click", "s_d")
#check the destination point, with actual click
dd = dd %>% as.data.frame()
dd = dd[dd$s_d == 1 & dd$click == 2,]
d_ori = dd[,4:5]

eps = 0.001 # for the granularity
set.seed(1)

test_likelihood_chengdu = cbind(rep(0, 100), rep(0, 100))
for(iter in 1:100){
  # only use part of the data to speed up
  d = d_ori[sample(nrow(d_ori), size = 0.1 * nrow(d_ori)), ]
  train_index = sample(nrow(d), size=0.8 * nrow(d), replace = F)
  test_index = setdiff(1:nrow(d), train_index)
  
  # get the results of KDE
  H = Hpi(d[train_index,])
  kde_res = kde(d[train_index,], H=H, gridsize = c(diff(range(d[,1]))/eps,
                                                   diff(range(d[,2]))/eps),
                binned = F)
  
  # get results of PALM
  res = trainPALM(d[train_index,], eps = 0.001, Kmax = 300, start_dim = 1)
  
  test_likelihood_kde = predict(kde_res, x = d[test_index,])
  sum_log_likelihood_kde = log2(test_likelihood_kde + 1) %>% sum
  
  test_likelihood = get_likelihood(res, test_index) %>% .[,4]
  sum_log_likelihood_palm = log2(test_likelihood + 1) %>% sum
  
  print(c("sum_log_likelihood_kde", "sum_log_likelihood_palm"))
  print(c(sum_log_likelihood_kde, sum_log_likelihood_palm))
  test_likelihood_chengdu[iter,] = c(sum_log_likelihood_kde, sum_log_likelihood_palm)
  write.csv(test_likelihood_chengdu, "./case/POI_retrieval_data/test_likelihood_chengdu.csv")
}

