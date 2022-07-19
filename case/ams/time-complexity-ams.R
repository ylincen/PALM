rm(list = ls())

#setwd("~/Dropbox/PALM-code-MLJ/")
source("./load_sources.R")
source("./case/DinoFun/mini1/explore-utils.R")
source("./get_bins.R")

d = read.csv("./case/ams/listings_summary.csv")
d = d[,7:8]
d = d[,c(2,1)]
t0 = proc.time()[3]
res = trainPALM(d, eps = 0.001, Kmax = 300, start_dim = 1)
t1 = proc.time()[3] - t0
print(t1)
nrow(d)

require(ks)
eps = 0.001
t0 = proc.time()[3]
H = Hpi(d, binned=F)
res = kde(d, H, gridsize = c(diff(range(d[,1]))/eps,
                             diff(range(d[,2]))/eps), 
          binned = F)
t2 = proc.time()[3] - t0
print(t2)