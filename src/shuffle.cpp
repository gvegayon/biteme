#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export(name = ".shuffle_bites")]]
List shuffle_bites(
  arma::imat data
) {

  // Getting unique ids
  arma::ivec ids = unique(data.col(1u));

  // Computing intervals
  for (unsigned int id = 0u; id < ids.size(); id++) {

    // Getting vector of indices
    arma::uvec idx = arma::find(data.col(1u) == ids.at(id));

    // If only one observation, then we just continue
    if (idx.size() <= 2u)
      continue;

    // Vector of intervals
    arma::ivec intervals(idx.size() - 1u);

    // Computing intervals
    for (unsigned int i = 0u; i < intervals.size(); i++)
      intervals.at(i) = data.at(idx.at(i + 1u) , 0u) - data.at(idx.at(i), 0u);

    // shuffling and accumulating to get the new times
    // We add the first bit so everything starts from it
    intervals = arma::cumsum(arma::shuffle(intervals)) + data.at(idx.at(0u), 0u);

    // Rewriting the new times
    for (unsigned int i = 0u; i < intervals.size(); i++)
      data.at(idx.at(i + 1u), 0u) = intervals.at(i);

  }

  // Resorting the data
  arma::uvec neworder = arma::stable_sort_index(data.col(0u));

  data = data.rows(neworder);
  return List::create(
    _["times"] = data,
    _["ord"]   = neworder
  );

}
