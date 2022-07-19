rm(list = ls())

#setwd("~/Dropbox/PALM-code-MLJ/")
source("./load_sources.R")
source("./case/DinoFun/mini1/explore-utils.R")
source("./get_bins.R")

require(ks)
require(igraph)
require(data.table)
require(png)

d_full = fread("./case/DinoFun/mini1/MC1 Data June 2015 V3/park-movement-Sat.csv")
d_full$mins = extract_mins(d_full); d_full$hs = extract_hs(d_full)
d_full = d_full[d_full$type == "movement", ]
d = cbind(d_full$X, d_full$Y)

eps = 1
t0 = proc.time()[3]
H = Hpi(d)
kde_res_full = kde(d,
                   gridsize = c(diff(range(d[,1]))/eps,
                                diff(range(d[,2]))/eps), 
                   binned = F, H=H)
t1 = proc.time()[3] - t0
save(list=c("kde_res_full", "t1"), file ="./case/DinoFun/mini1/kde_full_res.Rdata")

visualize_kde = function(kde_res){
  d_frame = as.data.frame(d); colnames(d_frame) = c("x", "y")
  img = "./case/DinoFun/mini1/Auxiliary Files/Park-Map-bw.png"
  parkimg = readPNG(img)
  
  d_frame = expand.grid(kde_res$eval.points[[1]],
                        kde_res$eval.points[[2]])
  d_frame[,3] = kde_res$estimate %>% as.numeric()
  d_frame = data.frame(d_frame)
  colnames(d_frame) = c("x", "y", "density")
  g = ggplot(d_frame) + annotation_raster(parkimg, xmin = 0, xmax = 100, ymin = 0, ymax = 100) + 
    geom_raster(aes(x=x,y=y,fill=density, alpha=0.7) ) +
    scale_fill_gradientn(colours=c("darkgreen", "green", "greenyellow", "yellow",
                                   "orange", "red", "darkred"), name = "density") +
    scale_alpha(range = c(0, 1), guide = "none")+
    theme(axis.title = element_blank(), text = element_text(size = 12))
  return(g)
}
g = visualize_kde(kde_res_full)
ggsave("./case/DinoFun/mini1/kde_full.png", g, device="png",
       width = 25, height = 20, units = "cm")
