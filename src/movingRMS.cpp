#include <cmath>
#include <vector>
#include <Rcpp.h>


// [[Rcpp::export]]
Rcpp::NumericVector movingRMS(Rcpp::NumericVector input_data, int intwind) {
  int L = input_data.size();
  
  // Create an Rcpp::NumericVector to store the result
  Rcpp::NumericVector rms_array(L, NA_REAL);
  
  for (int i = 0; i < L; ++i) {
    int startI = std::max(1, i - intwind + 1);
    int endI = i;
    
    double sum = 0.0;
    for (int j = startI; j <= endI; ++j) {
      sum += std::abs(input_data[j] * input_data[j]);
    }
    
    rms_array[i] = std::sqrt(sum / (endI - startI + 1));
  }
  
  return rms_array;
}



