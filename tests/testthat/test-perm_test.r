context("Checking permutation")

test_that("Permutation is exact", {

  set.seed(111)

  dat <- cbind(
    time = cumsum(runif(40)),
    id   = 1
  )

  dat <- rbind(
    dat,
    cbind(
      time =dat[,1] + runif(40, 0, .1),
      id   = 2
    )
  )

  dat <- dat[order(dat[,1]),,drop=FALSE]
  ans <- perm_test(dat, timegap, R=500)

  expect_true(ans$pval["2->1"] < ans$pval["1->2"])

})
