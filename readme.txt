Code for the paper: Yang, Lincen, Mitra Baratchi, and Matthijs van Leeuwen. "Unsupervised discretization by two-dimensional mdl-based histogram." arXiv preprint arXiv:2006.01893 (2020). (Revision submitted for review to Springer Machine Learning Journal.)


General info: 
- The code is mostly written in R; a small part is written in C++ (via Rcpp), which corresponds to the dynamic programming algorithm; 
- The IPD code is from the author's website, which is written in Java; 
- For applying KDE on the case study datasets, we use the R package "ks";

===========================================================================================

Required packages: 
dplyr, ggplot2, gmp, MASS, MCMCpack, data.table, foreach, doParallel, ks, igraph, png

===========================================================================================

To run the experiment/case study code, you can (NOTE: the real-world datasets used for case study are too large to upload here, so I only upload the code. The link for downloading/requesting the data can be found in the paper):
- navigate to the project folder "PALM-code-MLJ" using the command line, and then use "Rscript filename". Or, 
- open the corresponding code using RStudio, but make sure to set the working directory to be the project folder, using setwd(). E.g., on my computer, it is setwd("~/Dropbox/PALM-code-MLJ"). 

===========================================================================================

Each folder exp1 -- exp5 within the experiment folder corresponds to one experiment, with the same order as is in the paper.

===========================================================================================

In case of any questions, please contact l.yang@liacs.leidenuniv.nl; 

