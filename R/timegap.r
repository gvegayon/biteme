#' Compute average time gap (time inverval) between ego-alter bites
#'
#' @param data A data.frame or numeric matrix. The first column should be time and
#' the second column the ids.
#' @param ... Further arguments passed to the method.
#' @return A vector of length `length(unique(data[,2]))` with average time between bites
#' of `i->j` and `j->i`
#' @export
timegap <- function(data, ...) UseMethod("timegap")

#' @export
#' @rdname timegap
timegap.data.frame <- function(data, ...) {

  .timegap(as.matrix(data[,1:2, drop=FALSE]))

}

#' @export
#' @rdname timegap
timegap.matrix <- function(data, ...) {
  .timegap(data[, 1:2])
}

