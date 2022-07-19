# run PALM on ams
rm(list = ls())

#setwd("~/Dropbox/PALM-code-MLJ/")
source("./load_sources.R")
source("./case/DinoFun/mini1/explore-utils.R")
source("./get_bins.R")
d = read.csv("./case/ams/listings_summary.csv")
d = d[,7:8]
d = d[,c(2,1)]

t0 = proc.time()[3]
res = trainPALM(d, eps = 0.001, Kmax = 300, start_dim = 2)
t1 = proc.time()[3]
save(list = c("res", "t1"), file = "./case/ams/palm_res_full_startdim2.Rdata")
