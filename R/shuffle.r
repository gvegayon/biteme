#' Shuffle bite data by sorting the intervals at individual level
#' @param data A two-column integer matrix. The first column holds the times,
#' while the second column holds the individual ids.
#' @return A shuffled version of the data
#' @export
shuffle_bites <- function(data) UseMethod("shuffle_bites")

#' @export
shuffle_bites.default <- function(data) {

  .shuffle_bites(data)

}


#' @export
shuffle_bites.data.frame <- function(data) {

  ans <- shuffle_bites(data[,1:2])

  list(
    data = data[ans$ord, , drop = FALSE],
    times = ans$times
  )

}
