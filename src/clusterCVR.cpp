// [[Rcpp::plugins(cpp11)]]
#include <RcppArmadillo.h>

// Correctly setup the build environment
// [[Rcpp::depends(RcppArmadillo)]]


using namespace Rcpp;


// (mu^{y = l})
// [[Rcpp::export]]
double mu_ind(double mu, int y, int l) {
  bool ind = (y == l);
  return pow(mu, ind);
}

// For a given individual i, compute the numerators of the
// responsibility parameter zeta_ik for a given cluster k,
// using the length-D vector of votes, each of which can take
// values in 0:L. This amounts to the product of taking the
// relevant mu to the power of each data point in y.
// [[Rcpp::export]]
NumericVector mu3_yvec(arma::cube mu, IntegerVector y) {
  // dimensions
  int K = mu.n_rows;
  int D = mu.n_cols;
  int L = (mu.n_slices - 1);

  NumericVector resp_i (K);

  for (int k = 0; k < K; k++) {
    arma::mat mu_k = mu.row(k);
    double total = 1;
    for (int  j = 0; j < D; ++j) {
      for (int l = 0; l <= L; ++l) {
        total *= mu_ind(mu_k(j, l), y[j], l);
      }
    }
    resp_i[k] = total;
  }

  return resp_i;
}

// back out availability of options given set m
// [[Rcpp::export]]
IntegerVector choice_ind(int M, int L) {
  IntegerVector set;

  if (M == 1)
    set = {1, 1, 0};
  if (M == 2)
    set = {1, 0, 1};
  if (M == 3)
    set = {1, 1, 1};

  if (L == 3)
    set.insert(L, 1);

  return set;
}

// use to match set
// [[Rcpp::export]]
IntegerVector choice_set(int M) {
  IntegerVector set;

  if (M == 1)
    set = {0,1};
  if (M == 2)
    set = {0,2};
  if (M == 3)
    set = {0,1,2};
  return set;
}

// same as mu_yvec but with varying choice set
// [[Rcpp::export]]
NumericVector mu3_yvec_vchoice(arma::cube mu, IntegerVector y, IntegerVector m) {
  // dimensions
  int K = mu.n_rows;
  int D = mu.n_cols;

  NumericVector resp_i (K);

  for (int k = 0; k < K; k++) {
    arma::mat mu_k = mu.row(k);
    double total = 1;

    for (int j = 0; j < D; j++) {
      IntegerVector set_m = choice_set(m[j]);

      // denominator
      double sum_mu = 0;
      for (int l : set_m) {
        sum_mu += mu_k(j, l);
      }

      // multiply through using denominator
      for(int l : set_m) {
        total *= mu_ind(mu_k(j, l)/sum_mu, y[j], l);
      }
    }
    resp_i[k] = total;
  }
  return resp_i;
}

// Evaluate log likelihood of varying choice multinomial logit
// @param par The parameter of interest. A (L+1)-length vector of mlogit
//  coefficients for cluster k and office j. The coefficient for the reference
//  category, corresponding to abstention, is abbreviated here. That coefficient
//  is constrained to be 0. Therefore, the log likelihood appends 0 to the beginning
//  each time.
// @param zeta_k N-length vector of posterior for membership in cluster k
// @param y_j N length vector of outcomes
// @param m_j N by (L + 1) matrix of missingness indicators
// @return When used via \code{optim}, the \code{par} command returns the MLE
// parameter. See example.
//
//
// [[Rcpp::export]]
double ll_vclogit_rcpp(
    NumericVector par,
    IntegerVector y_j,
    NumericVector zeta_k,
    NumericMatrix m_j) {

  int L = par.length() + 1;
  int N = y_j.length();

  double ll = 0;

  for (int i = 0; i < N; i++) {

    //denominator
    double exp_denom = m_j(i, 0);
    for (int l = 1; l < L; l++) {
      exp_denom += m_j(i, l)*exp(par[l - 1]);
    }

    //numerator
    // l = 0
    double exp_frac = 1.0/exp_denom;
    ll += m_j(i, 0)*zeta_k[i]*(y_j[i] == 0)*log(exp_frac);

    for (int l = 1; l < L; l++) {
      double exp_frac = exp(par[l - 1])/exp_denom;
      // Rcpp::Rcout << "i, l =" << i << " " <<  l << " " << exp_frac << std::endl;
      ll += m_j(i, l)*zeta_k[i]*(y_j[i] == l)*log(exp_frac);
    }
  }
  // Rcpp::Rcout << ll << std::endl;
  return -ll;
}


// Evaluate the gradient of the log likelihood at a particular point
// @importParams l_vclogit
// [[Rcpp::export]]
NumericVector grad_vclogit_rcpp(
    NumericVector par,
    IntegerVector y_j,
    NumericVector zeta_k,
    NumericMatrix m_j) {

  int L = par.length() + 1;
  int N = y_j.length();

  // the lth gradient
  NumericVector grad (L - 1);
  grad.fill(0);

  // the sum over i
  for (int i = 0; i < N; i++) {

    //denominator
    double exp_denom = m_j(i, 0);
    for (int il = 1; il < L; il++) {
      exp_denom += m_j(i, il)*exp(par[il - 1]);
    }

    for (int l = 1; l < L; l++) {
      //numerator
      double exp_frac = exp(par[l - 1])/exp_denom;
      double deriv_log = -exp_frac;
      if (y_j[i] == l) deriv_log = 1 + deriv_log;

      grad[l - 1] += m_j(i, l)*zeta_k[i]*deriv_log;
    }
  }
  // Rcpp::Rcout << grad << std::endl;
  return -grad;
}
