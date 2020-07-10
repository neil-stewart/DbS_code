# Load the decision by sampling library
source("DbS.R")

# 80% chance of 4000 otherwise nothing 
# vs 3000 for sure
c3 <- list(g3A=gamble(x=c(4000, 0), p=c(0.8, 0.2)),
	g3B=gamble(x=c(3000), p=c(1))
)
c3

# 20% chance of 4000 otherwise nothing 
# vs 25% chance of 3000 otherwise
c4 <- list(g4A=gamble(x=c(4000, 0), p=c(0.2, 0.8)),
	g4B=gamble(x=c(3000, 0), p=c(0.25, 0.75))
)
c4

# Thresholds for discriminating amounts and probabilities
thresholds <- list(x=10, p=.1)

# A uniform background context of amounts and probabilities
context <- list(x=seq(-6000,6000,1), p=seq(-1,1,.01))

# Probability of selecting each gamble for each choice
predictions <- DbS(choices=list(c3=c3, c4=c4), 
	thresholds=thresholds, 
	context=context, 
	prob.sample.context=.5, 
	prob.sample.amount=.5, 
	self.comparison=F, 
	choice.mechanism="accumulators",
	choice.threshold=5
)
predictions
