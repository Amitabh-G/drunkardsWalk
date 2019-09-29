#set.seed(111)
#set.seed(17)
set.seed(17)
tic <- Sys.time()
sim.binom.vec <- function(num_obs, num_trials, prob_each_trial ) {
  bernoulli.trial.vec <- rbinom(num_obs, num_trials, prob_each_trial)
  return(bernoulli.trial.vec)
}

sum.till.trial <- function(trial_vec_truncated) {
  length.trial <- length(trial_vec_truncated)
  sum.b <- sum(trial_vec_truncated)
  sum.a <- length.trial - sum(trial_vec_truncated)
  return(list(sum.b, sum.a))
}

is.even <- function(x) x %% 2 == 0

lead.changes.vec <- function(trial_vec) {
  leading.movie.vec <- rep(0, length(trial_vec))
  if (trial_vec[1] == 0) {
    leading.movie.vec[1] <- "A"
  } else {
    leading.movie.vec[1] <- "B"
  }
  for (i in 2:length(trial_vec)) {
    trial.vec.truncated <- trial_vec[1:i]
    sums <- sum.till.trial(trial_vec_truncated=trial.vec.truncated)
    sum.b <- sums[[1]]
    sum.a <- sums[[2]]
    if (is.even(i)) {
      if (sum.b > sum.a) {
        leading.movie.vec[i] <- "B"
      } else if (sum.b == sum.a) {
        if (trial.vec.truncated[i] == 0) {
          leading.movie.vec[i] <- "A"
        } else {
          leading.movie.vec[i] <- "B"
        }
        leading.movie.vec[i] <- "A"
      } else {
        leading.movie.vec[i] <- "A"
      }
    } else {
      if (sum.b > sum.a) {
        leading.movie.vec[i] <- "B"
      } else {
        leading.movie.vec[i] <- "A"
      }
    }
  }
  return(leading.movie.vec)
}

num.change.in.trial <- function(leading.movie.vec) {
  num.change <- 0
  for (i in 1:length(leading.movie.vec)) {
    if (i != length(leading.movie.vec)){
      j = i + 1
      if (leading.movie.vec[j] != leading.movie.vec[i]) {
        num.change <- num.change + 1
      } else {
        num.change = num.change
      } 
    }
  }
  return(num.change)
} 

dist.of.changes <- function(num.sim, num_obs, num_trials, prob_each_trial) {
  num.change.vec <- rep(0, num.sim)
  for (i in 1:num.sim) {
    leading.movie.vec <- lead.changes.vec(sim.binom.vec(num_obs=20000, num_trials=1, prob_each_trial=0.5))
    num.change.vec[i] <- num.change.in.trial(leading.movie.vec)
  }
  return(num.change.vec)
}

prob.of.no.change <- function(num.changes.vec) {
  zero.ctr = 0
  for (i in 1:length(num.changes.vec)) {
    if (num.changes.vec[i] == 0) {
      zero.ctr = zero.ctr + 1
    } else {
      NA
    }
  }
  prob.no.chnge = zero.ctr/length(num.changes.vec)
  print(zero.ctr)
}
num.sim <- 100000
num.changes.vec <- dist.of.changes(num.sim=num.sim, num_obs=20000, num_trials=1, prob_each_trial=0.5)
top.ten.num.changes <- data.frame(sort(table(num.changes.vec),decreasing=TRUE)[1:10])
prob.of.zero.change = prob.of.no.change(num.changes.vec=num.changes.vec)


length(num.changes.vec)
time.taken = Sys.time() - tic
print("Time taken to run the simulation:")
print(time.taken)
print(prob.of.zero.change)

#####################################
### Write simulation result to a file
#####################################

library(data.table)
working.dir <- "D:/other/Programming/R/drunkardsWalk/data/ch-1/movieLeadProb/"
dirpath <- paste0(working.dir, num.sim, "-sims")
dir.create(dirpath)
data.table::fwrite(list(num.changes.vec), file = paste0(dirpath,"/num-changes-vec.csv"))
data.table::fwrite(top.ten.num.changes, file = paste0(dirpath,"/top-ten-changes.csv"))


#######################
### Summary of the data
#######################
print(num.changes.vec)
hist(num.changes.vec)
mean(num.changes.vec)
sd(num.changes.vec)
mode(num.changes.vec)
range(num.changes.vec)
IQR(num.changes.vec)
fivenum(num.changes.vec)
summary(num.changes.vec)
length(unique(num.changes.vec))
#############################################################################################
### Questions arising after the simulation: Discussion:
# 1) What distribution can be used to model the total number of changes of lead for a trial 
#     which can be defined as 20000 coin tosses, with equal probability of landing up heads?
#     We performed 10000 trials.
# 2) How does the author concude the data about the probability of no change = 88*probability 
#     of continuous seesaw of changes?
# 3) Find the distribution of the total change count? -- seems like an exponential dist from 
#     the histogram.
# 4) Finding the distribution of the leading movies -- means distribution over A and B and
#     probability mass over these two.
#       Will have two counts for each trial. Count of A and of B. If the count of each be 
#       summarised independently across the 100000 trials, it will most probably be uniform 
#       distribution. 
#################
### Plot the data
#################
library(ggplot2)
qplot(num.changes.vec, geom = "histogram", binwidth = 1)




