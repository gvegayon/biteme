#include <RcppArmadillo.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can

// [[Rcpp::export]]
NumericVector timegap(
  const NumericMatrix & data
) {

  NumericVector ans(2, 0.0);
  NumericVector n(2, 0.0);

  // If it is a single observation
  if (data.nrow() < 2u) {
    ans(0) = data(1, 0) - data(0, 0);
    return ans;
  }

  for (int i = 1; i < (int) data.nrow(); i++) {

    // Can't compare against myself
    if (data(i, 1) == data(i-1, 1))
      continue;

    if (data(i, 1) > data(i-1, 1)) {
      ans(0) += data(i, 0) - data(i-1, 0);
      ++n(0);
    } else {
      ans(1) += data(i, 0) - data(i-1, 0);
      ++n(1);
    }
  }

  return ans/n;

}
