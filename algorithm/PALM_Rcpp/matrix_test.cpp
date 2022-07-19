#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]

NumericMatrix getMat(NumericVector x){
	int lx = x.size();
	NumericMatrix M(lx,lx);


	for(int i = 0; i < lx; ++i){
		M(i, i) = log2(x[i]);
	}

	return M;



}