library(biteme)
library(dplyr)
library(magrittr)

fnames <- list.files("~/Dropbox/Bite data (deidentified)/All bites/", full.names = TRUE)
dyads  <- lapply(fnames, read_csv)

set.seed(1)
dat <- cbind(
  times = cumsum(sample.int(100, 10, TRUE)),
  ids   = sample.int(2, 10, TRUE)
  )

# Computing the interval sets
get_intervals <- function(x) {
  x %>%
    `colnames<-`(c("times", "ids")) %>%
    as.data.frame %>%
  group_by(ids) %>%
  mutate(
    interval  = times - lag(times)
  ) %>%
  split(f=.$ids) %>%
  lapply("[[", "interval") %>%
  lapply(sort)
}

get_intervals(dat)

ans <- shuffle_bites(dat)
get_intervals(ans)


dyads[[1]] %>%
  select(bite_sec, SubjectID) %>%
  as.matrix %>%
  shuffle_bites
