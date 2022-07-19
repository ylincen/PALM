rm(list = ls())
setwd("~/Dropbox/PALM-code-MLJ/")
source("load_sources.R")
source("./case/DinoFun/mini1/explore-utils.R")
source("./case/other_algs/visualize.R")
require(dplyr)

d = read.csv("./case/ams/listings_summary.csv")
d = d[,c("longitude", "latitude")]

source("./case/other_algs/run_ipd.R")
res_plot = run_ipd(d, eps = 0.001, Kmax = 300)
