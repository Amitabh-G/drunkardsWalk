##########################################################################
### Simulating the Monty-Hall problem's results for the strategy to change
##########################################################################
behind.door <- c("Car", "Goat", "Goat")
# The function that gives unusual output
generate.behind.door <- function(num.samples) {a <- replicate(n = num.samples, sample(behind.door, replace = FALSE)); return(a)}

# This gives expected output
sample.prizes <- function() { v <- sample(behind.door, replace = FALSE); return(v)}
generate.behind.door1 <- function(num.samples) {a <- replicate(n = num.samples, sample.prizes()); return(a)}
select.position <- function() {a <- sample(c(1,2,3), size = 1); return(a)}

show.goat <- function(selected.position, behind.doors) {
  positions <- c(1,2,3)
  remaining.positions <- positions[-c(selected.position)]
  goat.position <- remaining.positions[which(behind.doors[remaining.positions] == "Goat")]
  if (length(goat.position) == 1) {
    return(goat.position)
  } else {
    show.goat <- sample(goat.position, size = 1)
    return(show.goat)
  }
}

strategy.switch <- function(goat.shown, behind.doors, selected.position) {
  selected <- behind.doors[-c(selected.position,goat.shown)]
  return(selected)
}

strategy.not.switch <- function(goat.shown, behind.doors, selected.position) {
  selected <- behind.doors[selected.position]
  return(selected)
}

test.strategy <- function(num.sim) {
  success <- rep(0, num.sim)
  for (i in 1:num.sim) {
    selected.position = select.position()
    behind.doors <- generate.behind.door1(num.samples = 1)
    goat.shown <- show.goat(selected.position, behind.doors)
    car.or.goat <- strategy.switch(goat.shown, behind.doors, selected.position)
    if (car.or.goat == "Car") {
      success[i] <- 1
    }
  }
  success.num <- sum(success)
  prob.success <- success.num/num.sim
  return(prob.success)
}

prob.success <- test.strategy(1000000)
print(prob.success)











