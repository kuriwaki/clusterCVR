# cppFunction('NumericVector grad_vclogit_rcpp(
#     NumericVector par,
#     IntegerVector y_j,
#     NumericVector zeta_k,
#     NumericMatrix m_j) {
#
#   int L = par.length() - 1;
#   int N = y_j.length();
#
#   NumericVector grad (L + 1);
#
#   // the lth gradient
#   for (int l = 0; l <= L; l++) {
#
#   // the sum over i
#   for (int i = 0; i < N; i++) {
#
#     //denominator
#     double exp_denom = 0;
#     for (int l = 0; l <= L; l++) {
#         exp_denom += m_j(i, l)*exp(par[l]);
#     }
#
#     //numerator
#     double exp_frac = exp(par[l])/exp_denom;
#     grad[l] += m_j(i, l)*zeta_k[i]*(y_j[i] == l)*(1 - exp_frac);
#     }
#   }
#   return grad;
# }')
library(Rcpp)
cppFunction('int print_ncol(NumericMatrix m_j) {
  int L = m_j.ncol() - 1;
 return L;
 } ')



library(microbenchmark)
par <- c(1, 1)
zeta_k_0 <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.9)
y_j_0    <- c(0, 1, 0, 0, 1, 1, 0, 2, 1, 2)
m_j_0 <- matrix(
  c(1, 1, 0,
    1, 0, 1,
    1, 0, 1,
    1, 1, 1,
    1, 1, 1,
    1, 1, 1,
    1, 1, 1,
    1, 1, 1,
    1, 1, 1,
    1, 1, 1),
  ncol = 3,
  byrow = TRUE)

par_0 <- c(0, 1, 1)



ll_vclogit(c(1, 1), y_j_0, zeta_k_0, m_j_0)



g_l <- c()
l <- 3
for (i in 1:10) {
  exp_frac <- exp(par_0[l])/(sum(m_j_0[i, ]*exp(par_0)))
  g_l[i] <- sum(m_j_0[i, l]*zeta_k_0[i] *(y_j_0[i] == (l - 1))*(1 - exp_frac))
}
sum(g_l)
grad_vclogit(c(1, 1), y_j_0, zeta_k_0, m_j_0)



microbenchmark(ll_vclogit_R(par, y_j_0, zeta_k_0, m_j_0),
               ll_vclogit(par, y_j_0, zeta_k_0, m_j_0))
