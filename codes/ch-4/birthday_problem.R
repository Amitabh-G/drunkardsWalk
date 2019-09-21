# The birthday paradox

# Find n such that in a set of n randomly chosen people some pair of them will have the same birthday.

find.prob <- function(ctr) {
  days.year <- seq(365 - ctr, 365)
  days.year.fract <- days.year/365
  prob <- 1 - prod(days.year.fract)
  if (prob > 0.5) {
    n.found <- TRUE
    return(ctr)
  } else {
    ctr <- ctr + 1
    find.prob(ctr)
  }
}

birthday.prob <- function(ctr=NA) {
  n.found <- FALSE
  ctr <- 1
  prob <- find.prob(ctr)
}

print(birthday.prob())