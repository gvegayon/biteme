#' Shuffle bite data by sorting the intervals at individual level
#' @param data A two-column integer matrix. The first column holds the times,
#' while the second column holds the individual ids.
#' @return A shuffled version of the data
#' @export
#' @examples
#'
#' # Checking unbiasedness -----------------------------------------------------
#'
#' # In this example we permute bite data that has only 6 possible
#'
#' # Function to encode the shuffle
#' shuffle_wrap <- function(dat) {
#'   paste0(shuffle_bites(dat)$times[,1], collapse="")
#' }
#'
#' # Fake data. This has only 6 possible permutations
#' dat <- cbind(
#'   time = c(0, 1, 3, 6),
#'   ids  = rep(1, 4)
#' )
#'
#' # Tabulating and plotting the permutations
#' n <- 5e4
#' set.seed(111224)
#' ans <- replicate(n, shuffle_wrap(dat))
#' ans <- table(ans)/n
#'
#' ans
#' # ans
#' #    0136    0146    0236    0256    0346    0356
#' # 0.16842 0.16622 0.16618 0.16794 0.16566 0.16558
#'
#' # Plotting the distribution
#' barplot(ans)
#'
shuffle_bites <- function(data) UseMethod("shuffle_bites")

#' @export
shuffle_bites.default <- function(data) {

  .shuffle_bites(data)

}

#' @export
shuffle_bites.data.frame <- function(data) {

  ans <- data[, 1:2]
  ans[[1]] <- as.integer(ans[[1]])
  ans[[2]] <- as.integer(ans[[2]])

  ans <- .shuffle_bites(as.matrix(ans))

  # Sorting and restoring the data
  data[, 1:2] <- ans$times
  data[ans$ord + 1L, , drop=FALSE]


}

#' Permutation test
#' @param data A data set with at least 2 columns. The first columns holds times,
#' and the second column individual ids.
#' @param statistic A function that returns a vector. The statistic to compute
#' @param R Integer scalar. Number of replications.
#' @param ... Further arguments passed to `statistic`
#' @param ncores integer scalar. Number of cores to use.
#' @param cl An object of class cluster.
#' @details This function relies on the [shuffle_bites].
#' @export
perm_test <- function(
  data,
  statistic,
  R,
  ...,
  ncores = 1L,
  cl     = NULL
) {

  # Observed statistic
  ans0 <- statistic(data)

  if (ncores > 1L) {

    # If we need to create a cluster
    if (!length(cl)) {
      cl <- parallel::makeCluster(ncores)
      on.exit(parallel::stopCluster(cl))
    }

    # Loading the biteme package
    parallel::clusterEvalQ(cl, library(biteme))

    # Getting the simulation done
    ans <- parallel::parSapply(
      cl = cl,
      X  = seq_len(R),
      function(i, dat, statistic, ...) {
        statistic(shuffle_bites(dat))
      },
      dat  = data,
      stat = statistic,
      ...
      )

  } else {

    ans <- sapply(
      X  = seq_len(R),
      function(i, dat, statistic, ...) {
        statistic(shuffle_bites(dat))
      },
      dat  = data,
      stat = statistic,
      ...,
      simplify = TRUE
    )

  }

  # Coercing data
  ans <- matrix(ans, ncol = length(ans0), byrow = TRUE)

  # Computing pvalues
  pval <- colMeans(ans < matrix(ans0, ncol=ncol(ans), nrow = nrow(ans), byrow = TRUE))
  pval <- ifelse(pval > .5, 1 - pval, pval)

  structure(
    list(
      pval = pval,
      t0   = ans0,
      t    = ans
      ),
    class = "biteme_perm"
    )

}

#' @export
print.biteme_perm <- function(x, ...) {

  cat("\nPermutation test on bite data\n")
  cat(sprintf("Number of permutations: %i\n", nrow(x$t)))
  cat(sprintf("Pr(t0[%1$i] < t[%1$i]): %2$.4f\n", 1:length(x$pval), x$pval), append = TRUE)


}
