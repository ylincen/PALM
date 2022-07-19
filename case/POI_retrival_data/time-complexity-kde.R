# For the time of PALM, see run-palm.R

rm(list=ls())
setwd("~/Dropbox/PALM-code-MLJ/")
setwd("./case/POI_retrieval_data/")
require(ggplot2)
require(dplyr)
require(data.table)
source("../../case/DinoFun/mini1/explore-utils.R")

# get the data
d = fread("../../case/POI_retrieval_data/chengdu_train.txt")

dd = d[, c(1,3,4,7,8,9,10)]
colnames(dd) = c("ID", "s_long", "s_lat", "search_long", "search_lat",
                 "click", "s_d")
#check the destination point, with actual click
dd = dd %>% as.data.frame()
dd = dd[dd$s_d == 1 & dd$click == 2,]

require(ks)
eps = 0.001
d = dd[,4:5]
t0 = proc.time()[3]
H = Hpi(d, binned = F)
res = kde(d, H, gridsize = c(diff(range(d[,1]))/eps,
                             diff(range(d[,2]))/eps), 
          binned = F)
t1 = proc.time()[3] - t0