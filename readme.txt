The code is now available at: https://github.com/ylincen/PALM; 

General info: 
- The code is mostly written in R; a small part is written in C++ (via Rcpp), which corresponds to the dynamic programming algorithm; 
- The IPD code is from the author's website, which is written in Java; 
- For applying KDE on the case study datasets, we use the R package "ks";

===========================================================================================

Required packages: 
dplyr, ggplot2, gmp, MASS, MCMCpack, data.table, foreach, doParallel, ks, igraph, png

===========================================================================================

To run the experiment/case study code, you can: 
- navigate to the project folder "PALM-code-MLJ" using the command line, and then use "Rscript filename". Or, 
- open the corresponding code using RStudio, but make sure to set the working directory to be the project folder, using setwd(). E.g., on my computer, it is setwd("~/Dropbox/PALM-code-MLJ"). 

===========================================================================================

Each folder exp1 -- exp5 within the experiment folder corresponds to one experiment, with the same order as is in the paper.

===========================================================================================

The DinoFun / POI_retrieval_data / ams folder within the "case" folder corresponds to each 3 real-world dataset used in the paper, i.e., the amusement park data, the taxi destinations, and the Amsterdam housing data. 

===========================================================================================

In case of any questions, please contact l.yang@liacs.leidenuniv.nl; 

