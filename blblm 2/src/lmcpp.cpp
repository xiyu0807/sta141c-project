#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]
// estimate the regression estimates based on given the number of repetitions
//' @useDynLib blblm
//' @importFrom Rcpp sourceCpp
// [[Rcpp::export]]
List lmcpp(const arma::mat& X, const arma::colvec& y, const arma::colvec& w) {
  int n = X.n_rows, k = X.n_cols;
  arma::colvec coef = arma::solve(X, y); // fit model y ~ X
  arma::colvec res  = y - X*coef;  // residuals
  double s2 = std::inner_product(res.begin(), res.end(), res.begin(), 0.0)/(n - k);
  arma::colvec std_err = arma::sqrt(s2 * arma::diagvec(arma::pinv(arma::trans(X)*X)));
  return List::create(Named("coefficients") = coef,
                      Named("stderr") = std_err,
                      Named("rank") = k,
                      Named("residuals") = res,
                      Named("weights") = w);
  
}
