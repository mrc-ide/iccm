#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector competing_hazard(NumericMatrix x) {
  int nrow = x.nrow();
  int ncol = x.ncol();
  double p_occurs = 0;
  IntegerVector possible_outcomes = seq(1, ncol);
  NumericVector out(nrow);

  for (int i = 0; i < nrow; i++) {
    double row_product = 1;
    for (int j = 0; j < ncol; j++) {
      row_product *= 1 - x(i,j);
    }
    p_occurs = 1 - row_product;
    if(R::runif(0,1) < p_occurs){
      NumericVector probs = x(i, _);
      out[i] = Rcpp::as<double> (sample(possible_outcomes, 1, TRUE, probs));
    }
  }
  return out;
}
