
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
  op <- graphics::par(mar = rep(0, 4))
  on.exit(graphics::par(op))
  ran <- range(x[[1]], x[[2]])

  graphics::plot.new()
  graphics::plot.window(xlim = ran, ylim = c(.5, 2.5))

  xstart <- diff(ran)*.1+ran[1]



  cols <- c("steelblue", "tomato")
  for (i in 1:2) {
    graphics::text(x=xstart, y = i + .1, labels = paste("Subject", i))

    for (k in 1:length(x[[i]]))
      graphics::text(
        x = x[[i]][k],
        y = i,
        labels = substitute(t[i]^j, list(i=i, j=k)),
        pos = 1, offset = (k %% 2)*.75 + 1
        )

    lines(
      y = rep.int(i, length(x[[i]])), x = x[[i]], col=cols[i],
      type = "b",
      lwd=2
      )
  }


#
#
#   text(x=xstart, y = 2.1, labels = "Subject 2")
#   lines(y = rep.int(2, length(x[[2]])), x = x[[2]], col="tomato", type = "b",
#         lwd=2)

}
