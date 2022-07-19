rm(list = ls())

#setwd("~/Dropbox/PALM-code-MLJ/")
source("./load_sources.R")
source("./case/DinoFun/mini1/explore-utils.R")
source("./get_bins.R")

require(dplyr)
require(ks)
require(ggplot2)
require(MASS)
source("./case/ams/test_likelihood.R")




require(igraph)
require(data.table)
require(png)

d_full = fread("./case/DinoFun/mini1/MC1 Data June 2015 V3/park-movement-Sat.csv")
d_full$mins = extract_mins(d_full); d_full$hs = extract_hs(d_full)
d_full = d_full[d_full$type == "movement", ]
d = d_full[d_full$hs <= 5 & d_full$hs > 4,]
d = cbind(d$X, d$Y) %>% as.data.frame(); colnames(d) = c("X", "Y")
set.seed(1)
eps = 1
log_like_res = cbind(rep(0,100), rep(0,100))
for(iter in 1:100){
  cat("iter: ", iter)
  train_index = sample(nrow(d), size=0.8 * nrow(d), replace = F)
  test_index = setdiff(1:nrow(d), train_index)
  
  # palm
  res = trainPALM(d[train_index,], eps = 1, Kmax = 100, start_dim = 1)
  test_likelihood = get_likelihood(res, test_index) %>% .[,4]
  sum_log_likelihood_palm = log2(test_likelihood + 1) %>% sum
  
  # kde
  H = Hpi(d[train_index,])
  kde_res = kde(d[train_index,], H=H, gridsize = c(diff(range(d[,1]))/eps,
                                                   diff(range(d[,2]))/eps),
                binned = F)
  test_likelihood_kde = predict(kde_res, x = d[test_index,])
  sum_log_likelihood_kde = log2(test_likelihood_kde + 1) %>% sum
  print(sum_log_likelihood_palm - sum_log_likelihood_kde)
  log_like_res[iter,] = c(sum_log_likelihood_palm, sum_log_likelihood_kde)
}
write.csv(log_like_res, file="./case/DinoFun/mini1/log-likelihood-test-data.csv")
print((log_like_res[,1] - log_like_res[,2]) %>% mean)
print((log_like_res[,1] - log_like_res[,2]) %>% var)
