library(readr)
library(magrittr)
library(dplyr)
library(lubridate)
library(tidyr)

fnames <- list.files("~/Dropbox/Bite data (deidentified)/All bites/", full.names = TRUE)
dyads  <- lapply(fnames, read_csv)

dyads <- lapply(dyads, function(x) {
  x[[1]] <- as.integer(x[[1]])
  x
})
d <- as.matrix(dyads[[1]][,1:2])


set.seed(1)
ans0 <- shuffle_bites(d)

ids <- sort(unique(dyads[[1]]$SubjectID))
ids <- lapply(ids, function(x) which(dyads[[1]]$SubjectID == x) - 1L)
set.seed(1)
tmp <- biteme:::shuffle_bites_sorted(d, ids)
ans1 <- dyads[[1]]
ans1[,1:2] <- tmp$times
ans1 <- dyads[[1]][tmp$ord + 1L,]

ans0[,1:2] - ans1[,1:2]


stat <- function(x) {

  x <- x[dplyr::lag(x$SubjectID) != x$SubjectID,]
  x$slag <- dplyr::lag(x[[1]])
  mean(x[[1]] - x$slag, na.rm=TRUE)

}

i <- 13
ans0 <- stat(dyads[[i]])
ans <- perm_test(dyads[[i]], stat, 1000, ncores = 8)

z <- mean(ans0 < ans)
ifelse(z>.5, 1-z, z)

# How many times there's a chance of mimicry?
mimicry_prop <- function(x) {

  x %<>%
    arrange(BiteTime_hms) %>%
    transmute(
      consecutive_bite = SubjectID == lag(SubjectID)
    ) %>%
    group_by(consecutive_bite) %>%
    summarize(
      prop = n()
    ) %>%
    filter(!is.na(consecutive_bite)) %>%
    mutate(
      prop = prop/sum(prop)
    )

}

# Distribution
dat <- lapply(dyads, mimicry_prop) %>%
  bind_rows() %>%
  group_by(consecutive_bite) %>%
  summarize(
    mean = mean(prop),
    p05  = quantile(prop, .05),
    p95  = quantile(prop, .95),
    sd   = sd(prop)
  ) %T>%
  print

# How are the bite rates distributed?
bite_rates <- lapply(dyads, function(d) {
  d %>%
    group_by(SubjectID) %>%
    mutate(
      BiteTime_hms = as.integer(BiteTime_hms)
    ) %>%
    summarise(
      bite_rate = n() / (max(BiteTime_hms) - min(BiteTime_hms))
    )

}) %>% bind_rows()

hist(bite_rates$bite_rate*60, breaks=20)
