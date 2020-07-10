# This file contains example source code for calling each of the key
# functions in the DbS.R library. 
#
# R CMD BATCH example_function_calls.R should produce example output

source("DbS.R")

# Gambles are represented as matrices, and are constructed using gamble().
# Each gamble branch is represented on a row
g1A <- gamble(x=c(2500, 2400, 0), p=c(0.33, 0.66, 0.01))
g1A
g1B <- gamble(x=c(2400), p=c(1))
g1B



# Choices are lists of gambles.
c1 <- list(g1A=g1A, g1B=g1B)
c1
c2 <- list(g2A=gamble(x=c(2500, 0), p=c(0.33, 0.67)),
	g2B=gamble(x=c(2400, 0), p=c(0.34, 0.66))
)
c2
c3 <- list(g3A=gamble(x=c(4000, 0), p=c(0.8, 0.2)),
	g3B=gamble(x=c(3000), p=c(1))
)
c3
choices <- list(c1=c1, c2=c2, c3=c3)



# attributes.from.gamble() extracts either probabilities or amounts
attributes.from.gamble(g1A, "x")
attributes.from.gamble(g1A, "p")



# attributes.from.choice() extracts either probabilities or amounts and
# returns a sorted vector
attributes.from.choice(c1, "x")
attributes.from.choice(c1, "p")



# valence.gamble() makes the probability of bad branches (i.e., those with
# amounts less than threshold) negative
valence.gamble(g1A, absolute.threshold=100)



# valence.choice() makes the probability of bad branches negative. For
# gains, amounts within threshold of the minimum amount are bad. For losses,
# amounts within threshold of the maximum are good. For mixed gambles,
# losses are bad. Note that the 
valence.choice(choice=c1, threshold=100)



# prob.favorable() returns the proportion of comparisons between the target
# and other attributes in which the target exceeds the other attributes by
# at least threshold
prob.favorable(target.attribute=10, other.attributes=c(7,8,9,11,12,13), threshold=1)



# prob.favorable.against.choice.and.context() returns the proportion of
# comparisons against both the other attributes in the choice and against a
# background distribution of attribute values. prob.sample.context gives the
# relatve probability of sampling from the background context rather than
# the other attributes in the choice
prob.favorable.against.choice.and.context(attribute=5,
	other.attributes=c(1,2,4,6,7,8), 
	context=1:10, 
	threshold=1,
	prob.sample.context=.5
)



# Thresholds for discriminating amounts and probabilities
thresholds <- list(x=100, p=.1)



# The background context of amounts and probabilities
context <- list(x=seq(-6000,6000,1), p=seq(-1,1,.01))



# prob.each.attribute.in.gamble.favorable() returns a matrix where each
# element is the probability that the corresponding attribute in the gamble
# will be compared favorably against the other attributes in the choice and
# the background context
g1A.v <- valence.gamble(g1A, absolute.threshold=100)
g1B.v <- valence.gamble(g1B, absolute.threshold=100)
prob.each.attribute.in.gamble.favorable(gamble=g1A.v, 
	other.gambles=list(g1A.v, g1B.v), 
	thresholds=thresholds, 
	context=context, 
	prob.sample.context=.5
)



# prob.each.gamble.in.choice.favorable() returns the average probability for
# each gamble, averaged over all attributes in the gamble, on a favorable
# comparison. self.comparison sets whether an attribute is compared with all
# attributes in a the choice, or only those of the other competing gambles
prob.each.gamble.in.choice.favorable(choice=list(g1A.v, g1B.v), 
	thresholds=thresholds, 
	context=context, 
	prob.sample.context=.5, 
	prob.sample.amount=.5, 
	self.comparison=T
)



# prob.random.walk() takes a vector indicating the probability of a step in
# each direction and returns the probability that each option will win the
# walk
prob.random.walk(prob.favorable=c(.3,.5), choice.threshold=5)



# DbS() takes a list of choices and, for each, returns in a list a vector
# giving the probability of each gamble being selected. Note the choice of
# either a random walk to threshold or an accumulator race to threshold
predictions <- DbS(choices=choices, 
	thresholds=thresholds, 
	context=context, 
	prob.sample.context=.5, 
	prob.sample.amount=.5, 
	self.comparison=T, 
	choice.mechanism="random walk",
	choice.threshold=5
)
predictions
# To get the probability of picking the first gamble for each choice:
sapply(predictions,function(z) {z[[1]]})
