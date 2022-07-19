rm(list = ls())
require(dplyr)
#setwd("~/Dropbox/PALM-code-MLJ/")


source('load_sources.R')
source('./case/DinoFun/mini1/explore-utils.R')
require(data.table)
d = fread("./case/POI_retrieval_data/chengdu_train.txt")

dd = d[, c(1,3,4,7,8,9,10)]
colnames(dd) = c("ID", "s_long", "s_lat", "search_long", "search_lat",
                 "click", "s_d")
#only the destination point, with actual click
dd = dd %>% as.data.frame()
dd = dd[dd$s_d == 1 & dd$click == 2,]
t0 = proc.time()
source("./get_bins.R")
res = trainPALM(dd[,4:5], eps = 0.001, Kmax = 300, start_dim = 1)
save(res, file = "./case/POI_retrieval_data/palmResChengdu_startdim1.Rdata")
print(proc.time() - t0)