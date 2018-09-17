#include <RcppArmadillo.h>
using namespace Rcpp;

//' @param A data.frame or numeric matrix. The first column should be time and
//' the second column the ids.
//' @noRd
// [[Rcpp::export(name = ".timegap")]]
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

  bool idscatched = false;
  std::vector< int > ids(2);
  for (int i = 1; i < (int) data.nrow(); i++) {

    // Can't compare against myself
    if (data(i, 1) == data(i-1, 1))
      continue;

    if (!idscatched) {
      ids[0] = data(i, 1);
      ids[1] = data(i - 1, 1);
      idscatched = true;
    }

    if (data(i, 1) > data(i-1, 1)) {
      ans(0) += data(i, 0) - data(i-1, 0);
      ++n(0);
    } else {
      ans(1) += data(i, 0) - data(i-1, 0);
      ++n(1);
    }
  }

  // StringVector names(2);
  // char buffer0[50], buffer1[50];
  // std::sprintf(&(buffer0[0]), "%i->%i", ids[0], ids[1]);
  // std::sprintf(&(buffer1[0]), "%i->%i", ids[1], ids[0]);
  //
  // names[0] = buffer0;
  // names[1] = buffer1;

  ans = ans/n;
  ans.attr("ids") = ids;

  return ans;

}
