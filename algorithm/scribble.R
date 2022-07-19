rm(list = ls())

n = 1000
eps = 0.001
Kmax = 50

# x = rnorm(n,0,1)
# y = rnorm(n,0,1)
# 
# d = cbind(x,y)
# 
# ## make a bounding box of d
# cut_quantile = 2
# d = d[d[,1] < cut_quantile & d[,1] > -cut_quantile &
#         d[,2] < cut_quantile & d[,2] > -cut_quantile,]

# generate data 
source("exp/cde/data_generator.R")
set.seed(1)
d1 = indep_gaussian(1000)


# get two-d joint density estimation
setwd("~/Documents/PALM_R/")
source("load_sources.R")
source("algorithm/get_bins.R")
source("utililities-for-experiments/get_densities_for_bins.R")
merged_bins = get_bins_PALM(data = d1, eps=eps, Kmax = Kmax, 
                            visual = T, return_mat = F)
merged_bins_mat = get_bins_mat(merged_bins)

y_value = 0
bins_covers_y = (merged_bins_mat[,3:4] - y_value) %>% 
  sign %>% apply(1, function(row) all(row == c(-1,1))) %>% 
  {merged_bins_mat[.,]}

x_ranges = bins_covers_y[,1:2] %>% as.numeric() %>% range()
xs = seq(x_ranges[1], x_ranges[2], by=0.01)
data_points = cbind(xs, y_value)

source("new-experiments/conditional-density-estimation/test-utils.R")
bins_contain_data_points = 
  get_which_bins(data_points, merged_bins_mat = merged_bins_mat[,1:4])
joint_densities = merged_bins_mat[bins_contain_data_points,5]


plot(data_points[,1], joint_densities / dnorm(data_points[,1],0,1))
points(data_points[,1], dnorm(data_points[,2],0,1), col="red")

# get one-d marginal density estimation
source("utililities-for-experiments/get_density.R")
marginal_density_x = get_density_all(xs = xs, bins = merged_bins,
                                     which_dim = 1)
plot(xs, abs(dnorm(xs, 0, 1) - marginal_density_x))

diff_to_true_marginal = abs(dnorm(xs, 0, 1) - marginal_density_x) %>% mean()
diff_to_true_marginal

plot(data_points[,1], joint_densities / marginal_density_x, ylim = c(0,0.5))
points(data_points[,1], dnorm(data_points[,2],0,1), col="red")

# get one-d density estimation by directly fit a histogram model
bins_oned = get_bins_PALM(data = t(t(x)), eps=eps, Kmax = Kmax, 
                          visual = F, return_mat = F, force_not_to_merge = T)
direct_density_x = get_density_all(xs = xs, bins = bins_oned, which_dim = 1)
diff_to_true_direct = abs(dnorm(xs, 0, 1) - direct_density_x) %>% mean()
diff_to_true_direct

points(data_points[,1], joint_densities / direct_density_x, ylim = c(0,0.5),
       col = "blue")

diff_marginal_direct = abs(direct_density_x - marginal_density_x) %>% mean()
diff_marginal_direct

plot(xs, joint_densities / direct_density_x - dnorm(rep(0, length(xs)), 0, 1))
# Why is evaluating the density estimation a problem?
### for new data points near the boundary, the grid-histogram is more stable?


### it is hard to intuitively estimate the "numbers" for density estimation
###     say, is 0.1 a big difference??


### This division is also tricky as a small change in marginal density may
###    cause a huge difference in the CDE estimation.


### How to do extrapolation? As grid-histogram tends to cover more space..

### Does the evaluation of DE/CDE need train/test split?

