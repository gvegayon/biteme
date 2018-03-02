#' Shuffle bite data by sorting the intervals at individual level
#' @param data A two-column integer matrix. The first column holds the times,
#' while the second column holds the individual ids.
#' @param locations A list of length `unique(data[,2])` with each individual's
#' observations row locations in `data`. For advance use only (see details).
#' @param ... Further arguments passed to the method.
#' @details
#'
#' When `locations` is not provided, the algorithm by default generates the `locations`
#' list. The structure of such list should be 1 element per individual, and
#' for each individual an integer vector with positions from `0:(nrow(data) - 1)`
#' (indexing from 0) of where their observations are located at. The advantage
#' of using this parameter is that we avoid sorting and finding such positions
#' each time that the algorithm is called, which can make it significantly faster
#'
#' ```
#' lapply(sort(unique(data[,2])) , function(x) which(data[,2] == x) - 1L)
#' ```
#'
#'
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
#'   paste0(shuffle_bites(dat)[,1], collapse="")
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
shuffle_bites <- function(data, ...) UseMethod("shuffle_bites")

#' @export
#' @rdname shuffle_bites
shuffle_bites.default <- function(
  data,
  locations = NULL,
  ...) {

  ans <- if (length(locations))
    .shuffle_bites_sorted(data, locations)
  else
    .shuffle_bites_unsorted(data)

  ans$times[ans$ord + 1, , drop=FALSE]

}

#' @param coerce Logical scalar. When `TRUE` it makes sure that the first two
#' columns of the data are integer. You can skip this if you know that the
#' columns are integer (runs faster).
#' @param return.raw Logical scalar. When `TRUE` it returns the shuffled version
#' of the data (first two columns only) (also runs faster).
#' @rdname shuffle_bites
#' @export
shuffle_bites.data.frame <- function(
  data,
  locations  = NULL,
  coerce     = TRUE,
  return.raw = FALSE,
  ...
  ) {

  if (coerce) {
    ans <- data[, 1:2]
    ans[[1]] <- as.integer(ans[[1]])
    ans[[2]] <- as.integer(ans[[2]])
  }

  ans <- if (length(locations))
    .shuffle_bites_sorted(as.matrix(data), locations)
  else
    .shuffle_bites_unsorted(as.matrix(data))

  # Sorting and restoring the data
  if (!return.raw) {
    data[, 1:2] <- ans$times
    data[ans$ord + 1L, , drop=FALSE]
  } else {
    ans$times[ans$ord + 1, , drop=FALSE]
  }

}

#' Permutation test
#' @param data A data set with at least 2 columns. The first columns holds times,
#' and the second column individual ids.
#' @param statistic A function that returns a vector. The statistic to compute
#' @param R Integer scalar. Number of replications.
#' @param ... Further arguments passed to `statistic`
#' @param shuffle.args List of aditional parameters passed to [shuffle_bites].
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
  cl     = NULL,
  shuffle.args = list()
) {

  # Observed statistic
  ans0 <- statistic(data)

  # Obtaining locations
  locations <- lapply(
    sort(unique(data[,2,drop=TRUE])),
    function(x) {
      which(data[,2L,drop=TRUE] == x) - 1L
    }
    )

  if (ncores > 1L | length(cl)) {

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
      function(i, dat, statistic, loc, shuf, ...) {
        statistic(do.call(shuffle_bites, c(list(dat, loc), shuf)))
      },
      dat  = data,
      stat = statistic,
      loc  = locations,
      shuf = shuffle.args,
      ...
      )

  } else {

    ans <- sapply(
      X  = seq_len(R),
      function(i, dat, statistic, loc, shuf, ...) {
        statistic(do.call(shuffle_bites, c(list(dat, loc), shuf)))
      },
      dat  = data,
      stat = statistic,
      loc  = locations,
      shuf = shuffle.args,
      ...,
      simplify = TRUE
    )

  }

  # Coercing data
  ans <- matrix(ans, ncol = length(ans0), byrow = TRUE)

  # Computing pvalues
  pval <- colMeans(ans < matrix(ans0, ncol=ncol(ans), nrow = nrow(ans), byrow = TRUE))
  pval <- ifelse(pval > .5, 1 - pval, pval)

  # Checking names
  if (!length(names(ans0)))
    names(ans0) <- sprintf("stat%02i", 1L:length(ans0))

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
  cat(sprintf("Number of permutations: %i\n ", nrow(x$t)))
  cat(sprintf("Pr(t0[%1$i] < t[%1$i]): %2$.4f\n", 1:length(x$pval), x$pval), append = TRUE)


}

#' @export
plot.biteme_perm <- function(
  x,
  y = NULL,
  main = names(x$t0),
  ylab = "Frequency",
  xlab = "Statistic",
  border = NA,
  col = "steelblue",
  ...) {

  k <- length(x$pval)

  if (k > 1L) {
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar))
    graphics::par(mfrow = c(ceiling(k/2), 2))
  }

  for (i in seq_len(k)) {

    # Creating the histogram
    hist(x$t[, i], main = main[i], xlab = xlab, ylab = ylab,
         border = border, col = col, ...)

    # Adding lines and a nice legend
    abline(v = mean(x$t[,i]), lwd=2, lty = 1, col="black")
    abline(v = x$t0[i], lwd=2, lty = "dashed", col="tomato")


    legend(
      "topright",
      col = c("black", "tomato"),
      lty = c(1, 2),
      lwd = c(2,2),
      legend = c("Simulated", "Observed"),
      title = "Means", cex=.75, bty="n"
      )

    mtext(sprintf("p-val: %.4f, %i permutations", x$pval[i], nrow(x$t)), 3)

  }


}
