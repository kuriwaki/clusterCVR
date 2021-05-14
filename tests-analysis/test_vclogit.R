library(clusterCVR)

# clusterCVR MLE
regular_optim <- function(y = canada_mlogit_y, zeta = canada_mlogit_w, m = canada_mlogit_m) {
  optim(par = c(1, 1, 1),
        fn = ll_vclogit_R,
        y_j = y,
        zeta_k = zeta,
        m_j = m,
        method = 'BFGS',
        control = list(fnscale = -1))
}
# regular_optim()

fast_optim <- function(y = canada_mlogit_y, zeta = canada_mlogit_w, m = canada_mlogit_m) {
  optim(par = c(1, 1, 1),
        fn = ll_vclogit_rcpp,
        y_j = y,
        zeta_k = zeta,
        m_j = m,
        method = 'BFGS')
}
fast_optim()$par

fast_optim2 <- function(par = c(1, 1, 1), y = canada_mlogit_y, zeta = canada_mlogit_w, m = canada_mlogit_m) {
  optim(par = par,
        fn = ll_vclogit_rcpp,
        gr = grad_vclogit_rcpp,
        y_j = y,
        zeta_k = zeta,
        m_j = m,
        method = 'BFGS')
}
fast_optim2()$par


# mlogit replication ----
library(mlogit)
data(ModeCanada)


fit_mlogit <- function() {
  MC_mlogit <- mlogit.data(ModeCanada,
                           chid.var = "case",
                           alt.var = "alt",
                           drop.index = TRUE)
  mlogit(choice ~ 1,
         data = MC_mlogit,
         weights = dist,
         reflevel = "train")

}
coef(fit_mlogit())


# timing ---
microbenchmark(fast_optim2(),
               fast_optim2(c(1, -3, 1)),
               fast_optim(),
               fit_mlogit(),
               times = 20)
# Unit: milliseconds
# expr                           min        lq      mean    median        uq       max neval
# fast_optim2()             25.98897  28.60282  30.62040  31.71726  32.67819  33.83664    20
# fast_optim2(c(1, -3, 1))  15.07894  15.49445  17.84565  16.16144  20.77502  23.30931    20
# fast_optim()              54.28999  59.11924  59.73336  59.81979  60.76199  64.44371    20
# fit_mlogit()             203.91192 213.64770 228.71671 220.22669 234.94870 289.36428    20
regular_optim()$par
fast_optim2()$par
fast_optim()$par
coef(fit_mlogit())

