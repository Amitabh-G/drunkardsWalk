##############################################################
### Simulating Moris'chance of beating the record of Babe Ruth
##############################################################
# The probability of home run has been provided in the book The Drunkard's Walk
prob.home.run <- 1/14.7

# The data about the plate appearance has been taken from 
# https://www.baseball-reference.com/teams/NYY/1961.shtml
plate.appearance <- 698

num.Moris <- 10000 # Number of Moris type player simulations we want to do.
home.run.vec <- replicate(num.Moris, sum(rbinom(n=698, size = 1, prob = prob.home.run)))
record.breach <- sum(home.run.vec>60)
print(record.breach)
prob.of.record.breach <- record.breach/num.Moris
print(prob.of.record.breach)

#################
### Plot the data
#################
hist(home.run.vec, 
     main="Histogram for number of home runs", 
     xlab="Number of home runs", 
     border="blue", 
     col="green",
     breaks=60,
     freq = TRUE)