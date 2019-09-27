# Simulate German lottery from chapter-4, The Drunkard's Walk.

draw.lottery <- function(num.draws) {
  lottery.numbers <- seq(1, 49)
  lottery.draws <- matrix(nrow = num.draws, ncol = 6)
  for (i in 1:num.draws) {
    lottery.draws[i,] <- sample(lottery.numbers, 6, replace = TRUE)
  }
  return(lottery.draws)
}


do.experiment <- function(num.sim, num.draws) {
  repeat.vec <- rep(0, num.sim)
  for (i in 1:num.sim) {
    lottery.draws <- draw.lottery(num.draws = num.draws)
    repeat.vec[i] <- sum(duplicated(lottery.draws))
    # duplicated.lottery.draws <- lottery.draws[duplicated(lottery.draws) | duplicated(lottery.draws, fromLast=TRUE)]
  }
  return(repeat.vec)
}

repeat.vec <- do.experiment(num.sim = 100, num.draws = 3016)
sum(repeat.vec)
prob <- sum(repeat.vec)/length(repeat.vec)
prob

#####################################
### Write simulation result to a file
#####################################
library(data.table)
data.table::fwrite(list(repeat.vec), file = "D:/other/Programming/R/drunkardsWalk/data/ch-4/germanLottery/repeatVec-3016.csv")
