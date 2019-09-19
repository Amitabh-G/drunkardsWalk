##########################################################################
### Simulating the Monty-Hall problem's results for the strategy to change
##########################################################################
behind.door <- c("Car", "Goat", "Goat") # The things behind the doors.

sample.prizes <- function() { v <- sample(behind.door, replace = FALSE); return(v)} # Sample the things -- just randomize the order.
generate.behind.door <- function(num.samples) {a <- replicate(n = num.samples, sample.prizes()); return(a)} # Generate many samples of the things behing the door.
select.door <- function() {a <- sample(c(1,2,3), size = 1); return(a)} # Select one of the doors.

#' Show the goat every time -- exactly as the anchor does.
show.goat <- function(selected.door, things.behind.door) {
  positions <- c(1,2,3)
  remaining.positions <- positions[-c(selected.door)]
  goat.position <- remaining.positions[which(things.behind.door[remaining.positions] == "Goat")]
  if (length(goat.position) == 1) {
    return(goat.position)
  } else {
    show.goat <- sample(goat.position, size = 1)
    return(show.goat)
  }
}

#' The switch strategy.
strategy.switch <- function(goat.shown, things.behind.door, selected.door) {
  selected <- things.behind.door[-c(selected.door,goat.shown)]
  return(selected)
}

strategy.not.switch <- function(goat.shown, things.behind.door, selected.door) {
  selected <- things.behind.door[selected.door]
  return(selected)
}

#' See how the strategy performs. The simulations must be consistent with the theory.
test.strategy <- function(num.sim) {
  success <- rep(0, num.sim)
  for (i in 1:num.sim) {
    selected.door = select.door()
    things.behind.door <- generate.behind.door(num.samples = 1)
    goat.shown <- show.goat(selected.door, things.behind.door)
    car.or.goat <- strategy.switch(goat.shown, things.behind.door, selected.door)
    if (car.or.goat == "Car") {
      success[i] <- 1
    }
  }
  success.num <- sum(success)
  prob.success <- success.num/num.sim
  return(prob.success)
}
num.sim <- 1000000 # Higher number  leads to very near result to the theoretical answer.
prob.success <- test.strategy(num.sim=num.sim)
print(prob.success)
