# This scripts collects functions that can do density estimation using the PALM 
#   histogram. 
# The data flow is as follows:
# Learn the histograms (either PALM or grid-histogram); 
# Predict the density (joint, marginal, conditional) for (new) data points;
### Note that density estimation does not need to do the train/test split,
### as we always know the true density; or, if new data points come in, we can
### always re-fit a new histogram; 

### However, when we evaluate a density estimation method, we always integrate
### over all possible data points ...

### To compare: iterative MDL-histogram; DET (if we have the code); 
### NIPS that cited DET (if we have the code); separate MDL-histogram; 
### separate MDL-histogram with copula (if time allows); 


