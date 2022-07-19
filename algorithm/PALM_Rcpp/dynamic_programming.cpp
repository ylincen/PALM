#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;
using namespace std;


// [[Rcpp::export]]
List dynamicProgramming(NumericVector ne, double eps, int Kmax, int E, NumericVector C_til, double vols_other_dims, int n, bool eps_for_code_length, double C_0){
	NumericMatrix B(E, Kmax);
	NumericMatrix B_which(E, Kmax);

	if(!eps_for_code_length){
		eps = 1;
	}

	for(int i = 0; i < E; ++i){
		if(ne[i] == 0){
			B(i, 0) = 0;
		} else {
			B(i, 0) = - ne[i] * (log2(eps * ne[i]) - log2((C_til[i] - C_0) * n * vols_other_dims) );
		}
	}
  
  int which_minL;
	double candiL;
	
	for(int k = 1; k < Kmax; ++k){
		for(int i = k; i < E; ++i){
			double minL = std::numeric_limits<double>::infinity();
		
			for(int j = k - 1; j < i; ++j){
			  if(ne[i] - ne[j] == 0){
			    candiL = B(j,k-1);
			  } else{
			    candiL = - (ne[i] - ne[j]) * (
			      log2( eps * (ne[i] - ne[j]) ) - 
			        log2( (C_til[i] - C_til[j]) * n * vols_other_dims )
			    ) + B(j,k-1);
			  }
			  if(candiL < minL){
			    minL = candiL;
			    which_minL = j;
			  } 
			}
			
			B(i,k) = minL;
			B_which(i,k) = which_minL;

		}
	}
	
	return List::create(B, B_which);
}

