
#' Simulate a dyad
#' @param rates Numeric vector. Vector of rates
#' @param mimicy Numeric vector. proportion of mimicry (likelihood).
#' @param last.bite Numeric scalar. Time of last bite.
#' @export
simulate_dyad <- function(
  rates     = c(10, 15),
  mimicy    = c(.1, .5),
  last.bite = stats::runif(1, 1, 2)
) {

  structure(
    .simulate_dyad(rates, mimicy, last.bite),
    class = "bite_dyad"
  )
}

#' Plotting method
#' @param x An object of class `bite_dyad`
#' @param y Ignored.
#' @param ... Ignored.
#' @export
plot.bite_dyad <- function(x, y = NULL, ...) {

  # Computing time range
  ran <- range(x[[1]], x[[2]])

  plot.new()
  plot.window(xlim = ran, ylim = c(.5, 2.5))

  lines(y = rep.int(1, length(x[[1]])), x = x[[1]], col="steelblue", type = "b")
  lines(y = rep.int(2, length(x[[2]])), x = x[[2]], col="tomato", type = "b")

}
