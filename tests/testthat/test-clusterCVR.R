library(testthat)
context("clusterCVR tests ")

test_that("runs on sim data", {
  data("simdata_full")
  cl1 <- clusterCVR(simdata_full)
  cl1_again <- clusterCVR(simdata_full)
  expect_output(summary(cl1))


  simdata_noX <- simdata_full[names(simdata_full) != "X"]
  cl2 <- clusterCVR(simdata_noX)
  expect_output(summary(cl2))

  expect_true(identical(cl1$ests$mu, cl1_again$ests$mu))
  expect_false(identical(cl1, cl2))
})


test_that("subset works", {
  data("simdata_full")
  cl2 <- clusterCVR(simdata_full, subset = 1:100, fast = FALSE)
  cl3 <- clusterCVR(simdata_full, subset = 1:20, fast = TRUE, ignore_X = TRUE)
  cl4 <- clusterCVR(simdata_full,
                    subset = simdata_full$X[, 1] == TRUE,
                    fast = FALSE,
                    ignore_X = TRUE)
})


test_that("recode works", {
  data("simdata_full")
  foo <- simdata_full
  foo$y <- replace(foo$y,foo$y == 0,"abstain")
  foo$y <- replace(foo$y,foo$y == 1,"split")
  foo$y <- replace(foo$y,foo$y == 2,"straight")

  cl5 <- clusterCVR(foo, recode_key = c(abstain = 0, split = 1, straight = 2))
})

test_that("multiple runs", {
  cl6 <- clusterCVR(simdata_full, runs = 4)
  expect_equal(length(cl6$loglik_runs), 4)
  expect_equal(cl6$ests$loglik, max(cl6$loglik_runs))
})

