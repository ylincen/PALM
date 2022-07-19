#include <Rcpp.h>
#include <iostream>

using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]

void testInf(double x){

	double y = std::numeric_limits<double>::infinity();
	if(isinf(y)){
		cout << "yes Y! \n";
	}

	if(y > 3){
		cout << "yes Y again!";
	}

	if(isinf(x)){
		cout << "yes!";
	}

	if(x > 3){
		cout << "yes again!";
	}
}