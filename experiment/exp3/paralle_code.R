rm(list = ls())

#setwd("~/Dropbox/PALM-code-MLJ/")


#### Gauss
n_vec = c(5e3, 1e4,5e4)
eps = 0.001
Kmax = 50
num_iter = 500

df_list = as.list(1:500)
mer_list = as.list(1:500)
sls_list = as.list(1:500)

library(foreach)
library(doParallel)




for(n in n_vec){
   cat("working on n: ",n,"\n")
   cl<-makeCluster(10)
   registerDoParallel(cl)
   
   res_list = foreach(j = 1:num_iter, .packages = c("gmp","dplyr","igraph","ggplot2"),
                      .inorder = FALSE, .errorhandling = 'stop')%dopar%{
                         
                         cat("working on j = ",j,"\n")
                         
                         source("load_sources.R")
                         source("get_bins.R")
                         library(MASS)
                         # Multivariate normal distribution
                         data = mvrnorm(5*n, c(0,0), matrix(c(1,0.5,0.5,1), ncol = 2))
                         # data = mvrnorm(5*n, c(0,0), matrix(c(1,0.5,0.5,1), ncol = 2))
                         data = data[data[,1] > -5 & data[,1] < 5 & data[,2] > -5 & data[,2] < 5,]
                         data = data[1:n,]
                         
                         
                         #two-d hist
                         time1 = proc.time()
                         
                         res = trainPALM(data, eps, Kmax)
                         time2 = proc.time()
                         
                         merge_result = res$merged_res$g
                         
                         to_merge_learn = visual_2d_MDLhist(res, merge_result)
                         b = produce_short_line_segs(res$res_df, to_merge_learn)
                         sls = b %>% t() %>% .[,1:4] %>% as.data.frame()
                         list(df2, merge_result, sls)
                      }
   file_name = paste("./experiment/exp3/res_exp3_",n,".Rdata", sep = "")
   save(res_list, file = file_name)
   stopCluster(cl)
}
