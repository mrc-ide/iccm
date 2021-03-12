#include <Rcpp.h>
using namespace Rcpp;

//' Childs equilibrium number of previous infections
//'
//' @param age Age
//' @param sigma FOI
//' @param het Heterogeneity
//' @param vx vaccine impact wrt age (pre compute across whole age range)
//' @param mi Maternal immunity impact (pre computre across whole age range)
//' @param ii_shape Infection immunity shape parameter
//' @param ii_rate Infection immunity rate parameter
// [[Rcpp::export]]
double eq_prior_indiv(int age,
                     double sigma,
                     double het,
                     std::vector<double> vx,
                     std::vector<double> mi,
                     double ii_shape,
                     double ii_rate){

  // Inititalise equilibrium previous_infections
  std::vector<double> eq_infections(age, 0);
  // Inititalise first infections
  double gamma_scale = 1 / ii_rate;
  double ii = 1 - R::pgamma(0, ii_shape, gamma_scale, TRUE, FALSE);

  double cur_infections =  1 - exp(-sigma * het * mi[0] * ii * vx[0]);
  eq_infections[0] = cur_infections;

  // Cumulative total expected number of infections by day up to age of child
  for(int i = 1; i < age; ++i){
    ii = 1 - R::pgamma(eq_infections[i - 1], ii_shape, gamma_scale, TRUE, FALSE);
    //Rcout << "The value of eq : " << eq_infections[i - 1] << "\n";
    //Rcout << "The value of ii : " << ii << "\n";
    cur_infections =  1 - exp(-sigma * het * mi[i] * ii * vx[i]);
    eq_infections[i] = eq_infections[i - 1] + cur_infections;
  }
  return eq_infections[age - 1];
}
