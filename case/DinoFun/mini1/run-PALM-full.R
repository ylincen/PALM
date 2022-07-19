rm(list = ls())

#setwd("~/Dropbox/PALM-code-MLJ/")
source("./load_sources.R")
source("./case/DinoFun/mini1/explore-utils.R")
source("./get_bins.R")
require(igraph)
require(data.table)
require(png)

d_full = fread("./case/DinoFun/mini1/MC1 Data June 2015 V3/park-movement-Sat.csv")
d_full$mins = extract_mins(d_full); d_full$hs = extract_hs(d_full)
d_full = d_full[d_full$type == "movement", ]
d = cbind(d_full$X, d_full$Y)

eps = 1; Kmax = 100; 

t0 = proc.time()[3]
res = trainPALM(d,eps,Kmax,save_res = F, start_dim = 2)
t1 = proc.time()[3] - t0

save(list=c("res", "t1"), file="./case/DinoFun/mini1/palm_res_full_startdim2.Rdata")