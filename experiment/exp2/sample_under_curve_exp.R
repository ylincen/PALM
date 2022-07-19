rm(list = ls())

#setwd("~/Dropbox/PALM-code-MLJ/")
source("load_sources.R")
source("get_bins.R")
f = function(x, l = 2){
  return(sin(x*l*pi)/3 + 1/2)
}
n_vec = c(1e4,1e5,1e6)
l_vec = c(2,4,6)
n2_true = rep(0,15)
  

df_result_total = data_frame()
for(i in 1:5){
  cat("working on i = ", i, "\n")
  n = n_vec[i]

  for(j in 1:3){
    n1 = n
    n2 = n
    cat("working on j = ", j, "\n")
    #sample
    ll = l_vec[j]
    eps = 0.001
    Kmax = 50
    x = seq(0-eps/2,1+eps/2,by = eps)
    
    data = matrix(rep(0, 2*n1), ncol = 2)
    
    data[1:n1,1] = runif(n1,0-eps/2,1+eps/2)
    data[1:n1,2] = runif(n1,0-eps/2,1+eps/2)
    
    xx = runif(n2,0-eps/2,1+eps/2)
    yy = runif(n2,0-eps/2,1+eps/2)
  
    indexs = (yy < f(xx, ll))
    
    xx = xx[indexs]
    yy = yy[indexs]
    n2 = sum(indexs) 
    n2_true[i*3-3 + j] = n2
    data = rbind(data, cbind(xx, yy))
    d = data
    
    res = trainPALM(d, eps, Kmax)
    time2 = proc.time()
    
    merge_result = res$merged_res$g
    
    to_merge_learn = visual_2d_MDLhist(res, merge_result)
    b = produce_short_line_segs(res$res_df, to_merge_learn)
    
    #store the data
    df_result = t(b) %>% as.data.frame() %>% tbl_df
    colnames(df_result) = c("x","y","xend","yend","direction")
    df_result = mutate(df_result, n = n, l = ll)
    
    df_result_total = rbind(df_result_total, df_result)
  }
}
save(df_result_total,file = "./experiment/exp2/res/df_result_total_eps0.001.Rdata")


load("./experiment/exp2/res/df_result_total_eps0.001.Rdata")





n_vec = c(1e4,1e5,1e6)


l_vec = c(2,4,6)
df_line = data_frame()
for(i in 1:5){
  for(j in 1:3){
    n = n_vec[i]
    l = l_vec[j]
    x = seq(0-eps/2,1+eps/2,by = eps)
    y = f(seq(0-eps/2,1+eps/2,by = eps),l)
    
    df_here = cbind(x, y, n, l) %>% as.data.frame()
    
    df_line = rbind(df_line, df_here)
  }
}


ggplot(data = df_result_total) +
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend), size = 0.3) +
  geom_line(data = df_line, aes(x = x, y = y),col = "red" , alpha = 1, linetype = 1, size = 0.1) + 
  facet_grid(n~l) + theme_bw() +theme(axis.text = element_text(size = 6),
                                      axis.title = element_text(size = 6),
                                      legend.title = element_text(size = 6),
                                      legend.text = element_text(size = 6),
                                      plot.title = element_text(size = 6),
                                      legend.position = 'none')


ggplot(data = df_result_total %>% filter(n!=5e6)) +
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend), size = 0.3) +
  geom_line(data = df_line %>% filter(n!=5e6), 
            aes(x = x, y = y),col = "red" , alpha = 1, linetype = 1, size = 0.1) + 
  facet_grid(n~l, labeller = label_both) + 
  theme_bw() +theme(axis.text = element_text(size = 6),
                                      axis.title = element_text(size = 6),
                                      legend.title = element_text(size = 6),
                                      legend.text = element_text(size = 6),
                                      plot.title = element_text(size = 6),
                                      legend.position = 'none')

