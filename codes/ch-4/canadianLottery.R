# Simulating the canadian lottery. 


simulate.canadian.lottery <- function(num.trials) {
  sum.vec <- rep(0, num.trials)
  for (i in 1:num.trials) {
    lottery.numbers <- seq(1, 2400000, 1)
    sample.numbers <- sample(lottery.numbers, size = 500, replace = TRUE)
    sum.vec[i] <- sum(duplicated(sample.numbers))
  }
  return(sum.vec)
}

distribution.of.repeat <- function(num.experiments) {
  duplicated.sum <- rep(0, num.experiments)
  for (i in 1:num.experiments) {
    duplicated.entries.vector <- simulate.canadian.lottery(num.trials = 10000)
    duplicated.sum[i] <- sum(duplicated.entries.vector)
  }
  return(duplicated.sum)
}

duplicated.sum.vector <- distribution.of.repeat(num.experiments = 100)


