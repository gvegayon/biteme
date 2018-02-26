library(biteme)

coupuling <- function(x) {

  # Creating a data.frame
  x1 <- x[[1]][-1]
  x2 <- x[[2]][-1]

  dat <- data.frame(
    time = c(x1, x2),
    id   = c(rep.int(1L, length(x1)), rep.int(2L, length(x2)))
  )

  # Sorting
  dat <- dat[order(dat$time),]

  # Computing the statistic
  stat0 <- NULL
  stat1 <- NULL
  for (i in 2:nrow(dat))
    if (dat$id[i] != dat$id[i-1L])
      stat0 <- c(stat0, with(dat, time[i] - time[i-1L]))
    else
      stat1 <- c(stat1, with(dat, time[i] - time[i-1L]))
      # stat <- c(stat, with(dat, time[i] - time[i-1L]))
#
#   if (!length(stat0))
#     return(0)
#
#   if (!length(stat1))
#     return(100)

  # This statistic seems to work:
  # sum(dist(mimic))/(sum(dist(mimic)) + sum(dist(nomimic)))
  sum(stat0, na.rm = TRUE)/(sum(c(stat0, stat1), na.rm = TRUE))

}

# Simulations
n     <- 1e4
rates <- c(10, 10)
mimic <- c(.2, .2)

set.seed(1)
ans0 <- replicate(n, coupuling(simulate_dyad(rates, mimicy = c(0,0), last.bite = 60)))
ans0 <- unlist(ans0, TRUE)
set.seed(1)
ans1 <- replicate(n, coupuling(simulate_dyad(rates, mimicy = mimic, last.bite = 60)))
ans1 <- unlist(ans1, TRUE)

# Filtering
# ans1 <- ans1[ans1 < quantile(ans1, .95, na.rm = TRUE)]
# ans0 <- ans0[ans0 < quantile(ans0, .95, na.rm = TRUE)]

ran <- range(c(ans1, ans0), na.rm = TRUE)

hist(ans0, freq = FALSE, breaks = 100, col=adjustcolor("tomato", .6),
     border = "transparent", xlim = ran)
hist(ans1, freq = FALSE, col=adjustcolor("steelblue", .6), border = "transparent",
     breaks = 100, add=TRUE, xlim = ran)

ans <- qqplot(ans1, ans0)
with(ans, abline(lm(y~x)))
