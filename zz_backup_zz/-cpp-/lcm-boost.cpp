
#include <Rcpp.h>
#include <boost/integer/common_factor.hpp>  // included in BH  

// [[Rcpp::depends(BH)]]    

using namespace Rcpp;
 
// [[Rcpp::export]]
int computeGCD(int a, int b) {
    return boost::integer::gcd(a, b);
}

// [[Rcpp::export]]
int computeLCM(int a, int b) {
    return boost::integer::lcm(a, b);
}