#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(name = ".simulate_dyad")]]
std::vector< std::vector< double > > simulate_dyad(
  NumericVector rates,
  NumericVector mimicry,
  double meal_length = 1.0,
  int maxiter = 1000
) {


  // Initializing data
  std::vector< std::vector< double > > bites(2u);
  bites.at(0).push_back(0.0);
  bites.at(1).push_back(0.0);

  std::vector< bool > done(2);
  done.at(0) = false, done.at(1) = false;

  // Who will start
  int i = (
    unif_rand() < mimicry.at(0)/(mimicry.at(0) + mimicry.at(1))
  )? 0 : 1;
  int j = (i == 1)? 0 : 1;

  double t;
  int iter = 0;
  while (!done.at(0) | !done.at(1)) {

    // Should we stop?
    if (iter++ > maxiter)
      break;

    if (iter % 256 == 0)
      R_CheckUserInterrupt();

    // Should he mimic?
    if (unif_rand() < mimicry.at(i)) {

      // If so, has the other individual taken a bite?
      // if not, then switch places and make the other individual decide.
      if (bites.at(j).back() < bites.at(i).back()) {
        std::swap(i, j);
        continue;
      }

      // We'll start by him
      t = bites.at(j).back();

    } else
      t = bites.at(i).back();

    // If not mimicry, then
    t += Rf_rexp(rates.at(i));

    // Storing the information
    bites.at(i).push_back(t);

    // Did i reached the end? If so, then remove that last bit, and change the
    // leader
    if (bites.at(i).back() > meal_length) {

      done.at(i) = true;
      std::swap(i, j);
      continue;

    }

    // Swapping so the other individual has his turn, only if he has bite after
    // him
    if (bites.at(i).back() > bites.at(j).back())
      std::swap(i, j);

  }

  return bites;

}

