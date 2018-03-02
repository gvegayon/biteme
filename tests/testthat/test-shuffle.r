context("Checking shuffling function")

# Packages I depend on
suppressMessages({
  library(dplyr, quietly = TRUE)
  library(magrittr, quietly = TRUE)
})

# Function to generate random set of bites
gen_bites <- function(nrows = 10, nids = 2) {
  data.frame(
    times = cumsum(sample.int(100, nrows, TRUE)),
    ids   = sample.int(nids, nrows, TRUE)
  )
}

# Computing the interval sets
get_intervals <- function(x) {
  x %>%
    `colnames<-`(c("times", "ids")) %>%
    group_by(ids) %>%
    arrange(times) %>%
    mutate(
      interval  = times - lag(times)
      ) %>%
    split(f=.$ids) %>%
    lapply("[[", "interval") %>%
    lapply(sort)
}

test_that("it works", {

  # Generating data
  set.seed(6665)
  bites0 <- gen_bites(nrows = 100)
  bites1 <- shuffle_bites(bites0)

  ans0 <- get_intervals(bites0)
  ans1 <- get_intervals(bites1)

  # Intervals are preserved
  expect_equal(ans0, ans1)

  # Variables are different (at least one)
  expect_true(any(bites0$times != bites1[,1]))
  expect_true(any(bites0$ids != bites1[,2]))

})

# Checking uniformity ----------------------------------------------------------

# Function to encode the shuffle
shuffle_wrap <- function(dat) {
  paste0(shuffle_bites(dat)[,1], collapse="")
}

test_that("Is uniform", {

  # Deterministic data
  dat <- cbind(
    time = c(0, 1, 3, 6),
    ids  = rep(1, 4)
  )

  n <- 5e4
  ans <- replicate(n, shuffle_wrap(dat))
  ans <- table(ans)/n

  expect_equivalent(
    as.vector(ans), rep(1/6, 6), tolerance = .005)
})


test_that("Is uniform 2", {

  # Deterministic data
  dat <- cbind(
    time = c(0, 1, 3, 6, 10, 11),
    ids  = c(1, 1, 2, 1, 2, 2)
  )

  n <- 5e4
  ans <- replicate(n, shuffle_wrap(dat))
  ans <- table(ans)/n

  expect_equivalent(
    as.vector(ans), rep(1/4, 4), tolerance = .005)
})

