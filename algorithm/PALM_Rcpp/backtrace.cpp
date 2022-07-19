#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector backtrace(NumericMatrix B_which, int EE, int mm){
    NumericVector breakss(mm);
    for(int j = mm - 1; j > 0; j = j-1){
        breakss[j] = B_which(EE, j);
        EE = breakss[j];
    }

    return breakss;
}
